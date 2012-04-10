unit Twain_Wilson_Reg;

interface

procedure Register;

implementation

{$R Twain_Wilson.dcr}

uses Classes,
     cmpTwain;

procedure Register;
begin
  RegisterComponents ('Twain', [TcwTwain]);
end;



end.
