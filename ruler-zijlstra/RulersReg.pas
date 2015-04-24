unit RulersReg;

interface

uses Classes, Ruler, RERuler;

procedure Register;

implementation

{$R RulersReg.dcr}


procedure Register;
begin
  RegisterComponents('Rulers', [TRuler, TRERuler]);
end;

end.
 