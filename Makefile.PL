use strict;
use warnings;
use ExtUtils::MakeMaker;

WriteMakefile(
    NAME                => 'Finance::Bank::FR::SG::Pro',
    AUTHOR              => 'Patrick Mevzek <pmevzek@cpan.org>',
    VERSION_FROM        => 'lib/Finance/Bank/FR/SG/Pro.pm',
    ABSTRACT_FROM       => 'lib/Finance/Bank/FR/SG/Pro.pm',
    PL_FILES            => {},
    PREREQ_PM => {
        'WWW::Mechanize' => 1.04,
    },
    dist                => { COMPRESS => 'gzip -9f', SUFFIX => 'gz', },
    clean               => { FILES => 'Finance-Bank-FR-SG-Pro-*' },
);
