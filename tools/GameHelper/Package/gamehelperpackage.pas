{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit gamehelperpackage;

{$warn 5023 off : no warning about unused units}
interface

uses
  u_package_register, u_respons_commontype, form_gamehelper_connection, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('u_package_register', @u_package_register.Register);
end;

initialization
  RegisterPackage('gamehelperpackage', @Register);
end.
