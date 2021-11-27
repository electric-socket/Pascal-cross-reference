// Crossref - Pascal Cross reference
// Version 0.01
// January 9, 2021
// Copyright 2021 Paul Robinson

// Licensed under GPL v3 or at your option any later version

// Main Program - starts everything
program Crossref;

uses scan, setup;

Const
       VERSION_MAJOR             = 0;
       VERSION_RELEASE           = 0;
       VERSION_PATCH             = 0;
       VERSION_FULL              = VERSION_MAJOR*1000+
                                   VERSION_RELEASE *10+
                                   VERSION_PATCH;

// note, the folowing MUST be a string of digits in quotes
// as PROGRAM UPD does an auto-upddate on every compile

       VERSION_REV               = '1';

begin

    Init;
    Usage;
    OpenFile(PasPath);
    ScanFile;
    WriteFile;



end.

  
