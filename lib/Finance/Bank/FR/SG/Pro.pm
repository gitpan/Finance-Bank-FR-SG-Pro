## Finance::Bank::FR::SG::Pro : Perl interface to Société Générale online banking system for professional accounts
#
# Copyright (C) 2004 Patrick Mevzek
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or (at
# your option) any later version.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
# USA
#
####################################################################################################

package Finance::Bank::FR::SG::Pro;

use strict;

our $VERSION='0.1';

use Carp qw(carp croak);
use WWW::Mechanize;

## Login URL, as can be found in javascript on http://services.societegenerale.fr/professionnels/plan.htm
use constant BASE_URL => 'https://www.professionnels.secure.societegenerale.fr/EIOMain/1,,EIO_EIOHomePageGenerique_XX_HOMEP_HPSEC__,00.html';
use constant LOGIN_FORM_NAME => 'Login';


=pod

=head1 NAME

Finance::Bank::FR::SG::Pro - Check your Société Générale Professional accounts from Perl

=head1 VERSION

Version 0.1

=head1 SYNOPSIS

 use Finance::Bank::FR::SG::Pro;

 my $account=Finance::Bank::FR::SG::Pro->check_balance(
   username => "01234567",
   password => "123456",
   account => '11111 22222 333333333333', ## if not specified you get back an array with all accounts
  );

 print "BALANCE: ".$account->balance();
 foreach my $s ($account->statements())
 {
  print "Statement"
  print "\tType:".$s->type()."\n";
  print "\tDate:".$s->date()."\n";
  print "\tAmount:".$s->amount()."\n";
  print "\tDescription:".$s->description()."\n";
 }

=head1 DESCRIPTION

This module provides a rudimentary interface to the Société Générale 
Progéliance Net online banking system for professional accounts at
L<http://www.societegenerale.fr/>.
You will need either Crypt::SSLeay or IO::Socket::SSL installed for
HTTPS support to work with LWP.

The interface of this module is strongly modeled after the one from
Briac Pilpré's Finance::Bank::BNPParibas and Cédric Bouvier's
Finance::Bank::CreditMut.

All dates are normalized to the standard format: YYYY-MM-DD.
All amounts are given as proper numbers, with '.' as fractional separator.

=head1 WARNING

This is code for B<online banking>, and that means B<your money>, and that
means B<BE CAREFUL>. You are encouraged, nay, expected, to audit the source
of this module yourself to reassure yourself that I am not doing anything
untoward with your banking data. This software is useful to me, but is
provided under B<NO GUARANTEE>, explicit or implied.

=head1 METHODS

=head2 check_balance( username => $username, password => $password, account => $account)

Returns a list of account objects (Finance::Bank::SG::Pro::Account class)
if no account specified, or otherwise a single object being the account requested.
All parameters must be given as strings. Spaces in account number are irrelevant.

=cut

sub check_balance
{
 my ($class,%opts)=@_;
 croak "Must provide a password" unless exists $opts{password};
 croak "Must provide a username" unless exists $opts{username};

 my @accounts;

 $opts{ua} ||= WWW::Mechanize->new(
        agent      => "Finance::Bank::FR::SG::Pro/$VERSION ($^O)",
        cookie_jar => {},
 );

 my $self=bless({%opts},$class);

 my $r=$self->{ua}->get(BASE_URL); ## will trigger a redirect
 croak $r->error_as_HTML unless $self->{ua}->success();

 $self->{ua}->form_name(LOGIN_FORM_NAME) or croak "Cannot find the login form '" . LOGIN_FORM_NAME . "'";
 $self->{ua}->set_fields(USER=>$self->{username},PASSWORD=>$self->{password});

 $r=$self->{ua}->submit();
 croak $r->error_as_HTML unless $self->{ua}->success();

 my @l=$self->{ua}->find_link(url_regex=>qr/MenuID=SBORELCPT&PageID=Compte/) or croak "Cannot find link to account";
 croak "More than one link detected to accounts page, do not know what to do" unless @l==1;

 $r=$self->{ua}->get($l[0]->url());
 croak $r->error_as_HTML unless $self->{ua}->success();
 
 @l=$self->{ua}->find_link(url_regex=>qr/MenuID=SBOTELEX[ET]&PageID=Telechargement/) or croak "Cannot find link to account statements";
 croak "More than one link detected to statement accounts page, do not know what to do" unless @l==1;

 my $relurl=$l[0]->url();
 $r=$self->{ua}->get($relurl);
 croak $r->error_as_HTML unless $self->{ua}->success();
 
 ## Things are handled through different forms, with a javascript method to fill hidden forms in a last method
 ### Accounts are displayed in an IFRAME

 @l=$self->{ua}->find_link(url_regex=>qr/PageID=CompteTelecharge/) or croak "Cannot find link to iframe";
 croak "More than one link detected to iframe page, do not know what to do" unless @l==1;
 
 $r=$self->{ua}->get($l[0]->url());
 croak $r->error_as_HTML unless $self->{ua}->success();

 $self->{ua}->form_name('frameTelechargement') or croak "Cannot find the form to set the account";
 my @cpttype=$self->{ua}->current_form()->param('typeCompte');
 my @cptnum=$self->{ua}->current_form()->find_input('radioBouton','radio')->possible_values();

 if (defined($opts{account}) && $opts{account})
 {
  my $wacc=$opts{account};
  $wacc=~s/\s//g;
  croak "Account number $wacc not found"  unless grep { /^${wacc}$/ } @cptnum;
  @cptnum=($wacc);
 }
 
 my @a;
 foreach my $cpt (0..$#cptnum)
 {
  ### We go back to the ``main'' url to finally submit
  $r=$self->{ua}->get($relurl);
  croak $r->error_as_HTML unless $self->{ua}->success();

  ### Dates (we will retrieve the largest period possible)
  $self->{ua}->form_name('date') or croak "Cannot find the form to set the date";
  my $i1=$self->{ua}->current_form()->find_input('start') or croak "Can not find ``start'' popup";
  my $i2=$self->{ua}->current_form()->find_input('end')   or croak "Can not find ``end'' popup";
 
  my @i1=$i1->possible_values();
  my @i2=$i2->possible_values();

  $self->{ua}->form_name('formTelecharger') or croak "Cannot find the mail form to retrieve accounts";
 
  {
   local $^W=0; ## changing hidden fields triggers a warning
   $self->{ua}->set_fields('DateStart' => shift(@i1),
                           'DateEnd'   => pop(@i2),
                           'Compte'    => $cptnum[$cpt],
                           'TypeCompte'=> $cpttype[$cpt],
                          );
  }
  $self->{ua}->submit();
  croak $r->error_as_HTML unless $self->{ua}->success(); 
  push @a,Finance::Bank::FR::SG::Pro::Account->new($self->{ua}->content());
 }

 return wantarray()? @a : $a[0];
}

sub _normalize_date
{
 my $date=shift;
 my ($d,$m,$y)=split(/\//,$date);
 return "$y-$m-$d";
}

#########################################################
package Finance::Bank::FR::SG::Pro::Account;

use Carp qw(carp croak);

=pod

=head1 Account methods

=head2 sort_code()

Returns the sort code of the account. Currently, it returns an undefined value.

=head2 bank()

Returns the name of the bank.

=head2 name()

Returns the human-readable name of the account. We handle only accounts being of type 'CTE ENTR'.

=head2 account_no()

Returns the account number.

=head2 balance()

In scalar context, returns the balance of the account.
In list context, returns an array with the balance amount, and the balance date.

=head2 currency()

Returns the currency of the account.

=head2 statements()

Returns a list of statement objects (Finance::Bank::FR::SG::Pro::Statement class).

=cut

sub new
{
 my ($class,$csv)=@_;
 my @l=map { s/\s+$//; $_; } split(/\r?\n/,$csv);
 
 my %self=();
 $self{'bank'}=shift(@l);
 my @t=split(/;/,shift(@l));
 $t[0]=~s/\s+$//;
 ($self{'account_number'},$self{'account_name'})=@t;
 $self{'account_type'}=shift(@l);
 croak "We do not know how to handle account type ".$self{account_type} unless ($self{account_type} eq 'CTE ENTR');
 @t=split(/;/,shift(@l));
 $self{'balance_date'}=Finance::Bank::FR::SG::Pro::_normalize_date($t[1]);
 @t=split(/;/,shift(@l));
 $self{'balance_amount_currency'}=$t[2]; ## should be EUR
 $t[1]=~s/"//g; #"
 $t[1]=~s/,/./;
 $t[1]=~s/ //g;
 $self{'balance_amount'}=$t[1];

 shift(@l); ## empty line
 shift(@l); ## labels : Date;Nature de l'opération;Débit;Crédit;Monnaie;Date de valeur;Libellé interbancaire

 ## Push comments line back where they belong
 my (@el,@ll);
 foreach (reverse(@l))
 {
  if (/^;(.+)$/)
  {
   push @el,$1."\n";
   next;
  } else
  {
   push @ll,$_.";".(@el? join('',reverse(@el)) : '');
   @el=();
  }
 }
 my @statements;
 foreach (reverse(@ll)) { push @statements,Finance::Bank::FR::SG::Pro::Statement->new($_); }
 $self{'statements'}=[ @statements ];
 bless(\%self,$class);
 return \%self;
}

sub sort_code  { undef }
sub bank       { $_[0]->{bank} }
sub name       { $_[0]->{account_name} }
sub account_no { $_[0]->{account_number} }
sub balance    { wantarray()? ($_[0]->{balance_amount},$_[0]->{balance_date}) : $_[0]->{balance_amount} }
sub currency   { $_[0]->{balance_amount_currency} }
sub statements { @{ $_[0]->{statements} } }

#########################################################
package Finance::Bank::FR::SG::Pro::Statement;

use Carp qw(carp croak);

=pod

=head1 Statement methods

=head2 date()

Returns the date when the statement occured.

=head2 value_date()

Returns the date of value of the statement.

=head2 description()

Returns the unparsed description given by the bank for the statement.

=head2 description_details()

For some types of statement (see below), we do extra parsing of the description and/or fields.
This method returns an extra hashref with the result of the parse, as specified below.

For type ``CHEQUES'', the hashref contains up to two keys: check_number and bank_ref (optional).

For type ``PRELEVEMENTS'', the hasref contains three keys: number, by and details (being
an arrayref of lines parsed from the comment field).

For type ``COMMISSIONS ET FRAIS DIVERS'', the hashref may contain the following keys:
montant_ht (for the amount before taxes), montant_nt (for the amount without taxes),
tva being an hashref whose keys are the VAT percentage (as real number) and value
is the amount to be added for this VAT.

For type ``FACTURES CARTES PAYEES'', the hashref contains the following keys:
details (being an array ref of lines parsed from the comment field),
card_number (being the full card number used),
merchant_name (string),
operation_date
and for credit card payment abroad there are also those following keys:
debit_amount (true amount being paid initially)
debit_amount_currency (currency of the amount given in debit_amount)
transaction_fee (for the bank due to the currency conversion).

=head2 amount()

In scalar context, returns the amount of the statement.
In list context, returns an array with the amount, and the currency of the statement.

=head2 comment()

Returns the unparsed comment part of the statement.

=head2 type()

Returns the type of this statement.

=head2 as_string()

Returns a tab-delimited representation of the statement.
By default, it uses a tabulation to separate the fields, but the user
can provide its own separator.

=cut

sub new
{
 my ($class,$l)=@_;

 my @l=map { s/\s+$//; $_ } split(/;/,$l);
 croak "Got too few elements" unless (@l>=7);
 my $self={date => Finance::Bank::FR::SG::Pro::_normalize_date($l[0]),
           value_date => Finance::Bank::FR::SG::Pro::_normalize_date($l[5]),
           amount_currency => $l[4],
           description => $l[1],
           type => $l[6],
           comment => $l[7] || '',
          };

 my @ll=map { s/"//g; s/,/./; s/ //g; $_} @l[2..3];
 carp "Two amounts defined for ``$l[1]'' on $l[0] : $ll[0] in - and $ll[1] in +" if ($ll[0] && $ll[1]);
 carp "Positive amount for debit : $ll[0]"  if ($ll[0] && $ll[0] > 0);
 carp "Negative amount for credit : $ll[1]" if ($ll[1] && $ll[1] < 0);
 $self->{amount}=($ll[0])? $ll[0] : $ll[1];

 bless($self,$class);

 if ($self->{type}=~m/^CHEQUES/)
 {
  $self->{description_details}=$self->parse_cheque();
 } elsif ($self->{type}=~m/^COMMISSIONS ET FRAIS DIVERS$/)
 {
  $self->{description_details}=$self->parse_taxes();
 } elsif ($self->{type}=~m/^PRELEVEMENTS/)
 {
  $self->{description_details}=$self->parse_prelevement();
 } elsif ($self->{type}=~m/^FACTURES CARTES PAYEES$/)
 {
  $self->{description_details}=$self->parse_carte();
 } elsif ($self->{type}=~m/^ACHATS OPCVM$/)
 {
  $self->{description_details}=$self->parse_titre();
 }
 
 return $self;
}

sub date        { $_[0]->{date} }
sub value_date  { $_[0]->{value_date} }
sub description { $_[0]->{description} }
sub description_details { $_[0]->{description_details} } ## returns an hash reference
sub amount      { wantarray()? ($_[0]->{amount},$_[0]->{amount_currency}) : $_[0]->{amount} }
sub comment     { $_[0]->{comment} }
sub type	{ $_[0]->{type} }

sub as_string { 
	join ( $_[1] || "\t",  $_[0]->{date}, $_[0]->{description}, ($_[0]->{value_date} ||''), $_[0]->{amount}, $_[0]->{amount_currency} )
}

#################################################################################

sub parse_cheque
{
 my $self=shift;

 my ($checknb,$bankref)=($self->description()=~m/^CHEQUE\s+(\d+)\s+(\d+)$/);
 ## Bank ref does not seem to be always there...
 ($checknb,undef)=($self->description()=~m/^CHEQUE\s+(\d+)/) unless (defined($bankref) && $bankref);
 return { check_number => $checknb, bank_ref => $bankref };
}

sub parse_prelevement
{
 my $self=shift;

 my %r=( details => [ map { s/^"\s*//; s/\s*"$//; $_; } split(/\n/,$self->comment()) ] );
 ($r{number},$r{by})=($self->description()=~m/^PRELEVEMENT\s+(\d+)\s+(\S+)\s*$/);

 return \%r;
}

sub parse_carte
{
 my $self=shift;
 my @cl=map { s/^"\s*//; s/\s*"$//; $_; } split(/\n/,$self->comment());
 
 my %r=( details => \@cl );
 my $from;
 ($from,$r{card_number})=($self->description()=~m/^FAC\.(ETR\.|FRANCE)\s+(\d+)\s*$/);
 my $d;
 ($d,$r{merchant_name})=($cl[0]=~m/^(\S+)\s+(\S.*\S)\s*$/);
 my @d=split(/\//,$d);
 $r{operation_date}=(2000+$d[2])."-".$d[1]."-".$d[0];
 if ($from eq 'ETR.')
 {
  ($r{debit_amount},$r{debit_amount_currency})=($cl[1]=~m/^(\S+)\s+(\S+)\s+/);
  $r{debit_amount}=~s/,/./;
  ($r{transaction_fee})=($cl[2]=~m/^COMM\. ETRANGER\s+(\S+)E\//);
  $r{transaction_fee}=~s/,/./;
 }

 return \%r;
}


sub parse_taxes
{
 my $self=shift;
 my %r;
 foreach my $l (split(/\n/,$self->comment()))
 {
  if ($l=~m/^"?MONTANT (HT|NT)\s*:\s+(\d+,\d+) EUR\s*"?$/)
  {
   my $k='montant_'.lc($1);
   $r{$k}=$2;
   $r{$k}=~s/,/./;
  }
  if (my ($t1,$t2)=map { s/,/./; $_; } ($l=~m/^"?TVA A (\d+,\d+)[%\s]:\s+(\d+,\d+) EUR\s*"?$/))
  {
   $r{tva}->{$t1}=$t2;
  }
 }
 return \%r;
}

sub parse_titre
{
 my $self=shift;
 my %r;
 my @all=($self->description()=~m/^\s*(\S+)\s+(\d+)\s+(\S+)\s+(\S+)\s+\S+$/);

 ## TODO: will need to handle other cases... when we have examples !
 if (($all[0] eq 'SOU') && ($all[2] eq 'ACTIONS'))
 {
  $r{quantity}='+'.$all[1];
  $r{name}=$all[3];
 }

 return \%r;
}

#################################################################################

=head1 COPYRIGHT & LICENSE

Copyright 2004 Patrick Mevzek, All Rights Reserved.
Released under the GPL License.

=head1 AUTHOR

Patrick Mevzek C<< <pmevzek@cpan.org> >>

This work has been sponsored by and done for Dot and Co L<http://www.dotandco.com>.

Thanks to Andy Lester for its WWW::Mechanize wonder, and other authors of Finance::Bank::* modules

=head1 BUGS

Please report any bugs or feature requests to
C<bug-finance-bank-fr-sg-pro@rt.cpan.org>, or through the web interface at
L<http://rt.cpan.org>.  I will be notified, and then you'll automatically
be notified of progress on your bug as I make changes.

=head1 SEE ALSO

Finance::Bank::CreditMut, Finance::Bank::BNPParibas, WWW::Mechanize

=cut

1; # End of Finance::Bank::FR::SG::Pro
