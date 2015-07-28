unit HeaptrcLog;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, heaptrc;

implementation

initialization
  // Set up a logfile for heaptrc
  // We do this in initialization to get the log working as early as possible
  if FileExists('heaptrc.log') then
    DeleteFile('heaptrc.log');
  SetHeapTraceOutput('heaptrc.log');
end.

