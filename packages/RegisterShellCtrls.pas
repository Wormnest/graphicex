unit RegisterShellCtrls;

interface

procedure Register;

implementation

uses Classes, TypInfo, Controls, ShellCtrls, ShellConsts;

procedure Register;
begin
  RegisterComponents('Shell Controls', [TShellListView]);
  RegisterComponents('Shell Controls', [TShellTreeView]);
  RegisterComponents('Shell Controls', [TShellChangeNotifier]);
end;

end.
