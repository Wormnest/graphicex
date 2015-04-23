{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit rkViewFpc;

interface

uses
  rkIntegerList, rkView, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('rkView', @rkView.Register);
end;

initialization
  RegisterPackage('rkViewFpc', @Register);
end.
