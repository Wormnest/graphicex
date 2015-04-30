{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit TwainWilson_design;

interface

uses
  Twain_Wilson_Reg, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('Twain_Wilson_Reg', @Twain_Wilson_Reg.Register);
end;

initialization
  RegisterPackage('TwainWilson_design', @Register);
end.
