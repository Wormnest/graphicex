unit RulersReg;

interface

uses Classes, Ruler{$IFNDEF FPC} , RERuler {$ENDIF};

procedure Register;

implementation

{$R RulersReg.dcr}


procedure Register;
begin
  RegisterComponents('Rulers', [TRuler{$IFNDEF FPC} , TRERuler {$ENDIF}]);
end;

end.
 
