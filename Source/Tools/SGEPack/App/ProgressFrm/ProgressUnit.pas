unit ProgressUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, LCLType;

type
  TProgressFrm = class(TForm)
    ProgressBar: TProgressBar;
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private

  public
    procedure Reset;
    procedure SetUp(Count: Integer);
    procedure Step;
  end;

var
  ProgressFrm: TProgressFrm;

implementation

{$R *.lfm}


procedure TProgressFrm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_ESCAPE: Close;
  end;
end;


procedure TProgressFrm.Reset;
begin
  ProgressBar.Position := 0;
end;


procedure TProgressFrm.SetUp(Count: Integer);
begin
  Reset;
  ProgressBar.Max := Count;
  Show;
end;


procedure TProgressFrm.Step;
begin
  ProgressBar.StepIt;
end;


end.

