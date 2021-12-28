{
Пакет             Simple Game Engine 2
Файл              sgeVariables.pas
Версия            1.1
Создан            24.08.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Методы виртуальных переменных
}
{$Include Defines.inc}

unit sgeVariables;

{$mode objfpc}{$H+}

interface


procedure sgeVariables_Init(SGEObject: TObject);


implementation

uses
  SimpleGameEngine, sgeGraphicColor,
  sgeExtensionMusicPlayer;


var
  SGE: TSimpleGameEngine;


{$Region Shell}
procedure ExtensionShell_SetEnable(AEnable: Boolean);
begin
  SGE.ExtShell.Enable := AEnable;
end;

function ExtensionShell_GetEnable: Boolean;
begin
  Result := SGE.ExtShell.Enable;
end;


procedure ExtensionShell_SetBGColor(AColor: TsgeRGBA);
begin
  SGE.ExtShell.BGColor := sgeRGBAToColor(AColor);
end;

function ExtensionShell_GetBGColor: TsgeRGBA;
begin
  Result := sgeColorToRGBA(SGE.ExtShell.BGColor);
end;


procedure ExtensionShell_SetEditorTextColor(AColor: TsgeRGBA);
begin
  SGE.ExtShell.EditorTextColor := sgeRGBAToColor(AColor);
end;

function ExtensionShell_GetEditorTextColor: TsgeRGBA;
begin
  Result := sgeColorToRGBA(SGE.ExtShell.EditorTextColor);
end;


procedure ExtensionShell_SetEditorSelectColor(AColor: TsgeRGBA);
begin
  SGE.ExtShell.EditorSelectColor := sgeRGBAToColor(AColor);
end;

function ExtensionShell_GetEditorSelectColor: TsgeRGBA;
begin
  Result := sgeColorToRGBA(SGE.ExtShell.EditorSelectColor);
end;


procedure ExtensionShell_SetEditorCursorColor(AColor: TsgeRGBA);
begin
  SGE.ExtShell.EditorCursorColor := sgeRGBAToColor(AColor);
end;

function ExtensionShell_GetEditorCursorColor: TsgeRGBA;
begin
  Result := sgeColorToRGBA(SGE.ExtShell.EditorCursorColor);
end;


procedure ExtensionShell_SetErrorColor(AColor: TsgeRGBA);
begin
  SGE.ExtShell.ErrorColor := sgeRGBAToColor(AColor);
end;

function ExtensionShell_GetErrorColor: TsgeRGBA;
begin
  Result := sgeColorToRGBA(SGE.ExtShell.ErrorColor);
end;


procedure ExtensionShell_SetTextColor(AColor: TsgeRGBA);
begin
  SGE.ExtShell.TextColor := sgeRGBAToColor(AColor);
end;

function ExtensionShell_GetTextColor: TsgeRGBA;
begin
  Result := sgeColorToRGBA(SGE.ExtShell.TextColor);
end;


procedure ExtensionShell_SetNoteColor(AColor: TsgeRGBA);
begin
  SGE.ExtShell.NoteColor := sgeRGBAToColor(AColor);
end;

function ExtensionShell_GetNoteColor: TsgeRGBA;
begin
  Result := sgeColorToRGBA(SGE.ExtShell.NoteColor);
end;
{$EndRegion Shell}


{$Region Controllers}
procedure ExtensionControllers_SetEnable(AEnable: Boolean);
begin
  SGE.ExtControllers.Enable := AEnable;
end;

function ExtensionControllers_GetEnable: Boolean;
begin
  Result := SGE.ExtControllers.Enable;
end;


procedure ExtensionControllers_SetScanDelay(ADelay: Integer);
begin
  SGE.ExtControllers.ScanDelay := ADelay;
end;

function ExtensionControllers_GetScanDelay: Integer;
begin
  Result := SGE.ExtControllers.ScanDelay;
end;


procedure ExtensionControllers_SetAutoScan(AEnable: Boolean);
begin
  SGE.ExtControllers.AutoScan := AEnable;
end;

function ExtensionControllers_GetAutoScan: Boolean;
begin
  Result := SGE.ExtControllers.AutoScan;
end;


procedure ExtensionControllers_SetAutoScanDelay(ADelay: Integer);
begin
  SGE.ExtControllers.AutoScanDelay := ADelay;
end;

function ExtensionControllers_GetAutoScanDelay: Integer;
begin
  Result := SGE.ExtControllers.AutoScanDelay;
end;


function ExtensionControllers_GetCount: Integer;
begin
  Result := SGE.ExtControllers.ControllerList.Count;
end;
{$EndRegion Controllers}


{$Region Sound}
procedure ExtensionSound_SetVolume(AVolume: Single);
begin
  SGE.ExtSound.Sound.Gain := AVolume;
end;

function ExtensionSound_GetVolume: Single;
begin
  Result := SGE.ExtSound.Sound.Gain;
end;
{$EndRegion Sound}


{$Region Music}
procedure ExtensionMusic_SetVolume(AVolume: Single);
begin
  SGE.ExtMusicPlayer.Volume := AVolume;
end;

function ExtensionMusic_GetVolume: Single;
begin
  Result := SGE.ExtMusicPlayer.Volume;
end;


procedure ExtensionMusic_SetFadeTime(AFadeTime: Integer);
begin
  SGE.ExtMusicPlayer.FadeTime := AFadeTime;
end;

function ExtensionMusic_GetFadeTime: Integer;
begin
  Result := SGE.ExtMusicPlayer.FadeTime;
end;


procedure ExtensionMusic_SetChangeMode(AChangeMode: String);
var
  Cm: TsgeExtensionMusicPlayerChangeMode;
begin
  case AChangeMode of
    'Random'  : Cm := cmRandom;
    'Forward' : Cm := cmForward;
    'Backward': Cm := cmBackward;
  end;

  SGE.ExtMusicPlayer.ChangeMode := Cm;
end;

function ExtensionMusic_GetChangeMode: String;
begin
  case SGE.ExtMusicPlayer.ChangeMode of
    cmRandom  : Result := 'Random';
    cmForward : Result := 'Forward';
    cmBackward: Result := 'Backward';
  end;
end;


procedure ExtensionMusic_SetRepeatMode(ARepeatMode: String);
var
  Rm: TsgeExtensionMusicPlayerRepeatMode;
begin
  case ARepeatMode of
    'None'  : Rm := rmNone;
    'Track' : Rm := rmTrack;
    'List'  : Rm := rmList;
  end;

  SGE.ExtMusicPlayer.RepeatMode:= Rm;
end;

function ExtensionMusic_GetRepeatMode: String;
begin
  case SGE.ExtMusicPlayer.RepeatMode of
    rmNone  : Result := 'None';
    rmTrack : Result := 'Track';
    rmList  : Result := 'List';
  end;
end;


procedure ExtensionMusic_SetGroup(AGroup: String);
begin
  SGE.ExtMusicPlayer.Group := AGroup;
end;

function ExtensionMusic_GetGroup: String;
begin
  Result := SGE.ExtMusicPlayer.Group;
end;
{$EndRegion Music}


{$Region Music.TrackList}
function ExtensionMusic_TrackList_GetCurrent: String;
begin
  Result := '';
  if SGE.ExtMusicPlayer.TrackList.CurrentTrack <> nil then
    Result := SGE.ExtMusicPlayer.TrackList.CurrentTrack.Name;
end;
{$EndRegion Music.TrackList}



//////////////////////////////////////////////////
procedure sgeVariables_Init(SGEObject: TObject);
begin
  //Запомнить указатель
  SGE := TSimpleGameEngine(SGEObject);

  //Добавить переменные
  with SGE.ExtVariables do
    begin
    //Shell
    AddBoolean('Shell.Enable', False, @ExtensionShell_GetEnable, @ExtensionShell_SetEnable, 'On', 'Off');
    AddColor('Shell.BGColor', sgeGetRGBA(0, 0, 0, 128), @ExtensionShell_GetBGColor, @ExtensionShell_SetBGColor);
    AddColor('Shell.EditorTextColor', sgeGetRGBA(255, 255, 255, 255), @ExtensionShell_GetEditorTextColor, @ExtensionShell_SetEditorTextColor);
    AddColor('Shell.EditorSelectColor', sgeGetRGBA(255, 255, 255, 128), @ExtensionShell_GetEditorSelectColor, @ExtensionShell_SetEditorSelectColor);
    AddColor('Shell.EditorCursorColor', sgeGetRGBA(255, 255, 255, 255), @ExtensionShell_GetEditorCursorColor, @ExtensionShell_SetEditorCursorColor);
    AddColor('Shell.ErrorColor', sgeGetRGBA(255, 0, 0, 255), @ExtensionShell_GetErrorColor, @ExtensionShell_SetErrorColor);
    AddColor('Shell.TextColor', sgeGetRGBA(255, 255, 255, 255), @ExtensionShell_GetTextColor, @ExtensionShell_SetTextColor);
    AddColor('Shell.NoteColor', sgeGetRGBA(255, 255, 255, 128), @ExtensionShell_GetNoteColor, @ExtensionShell_SetNoteColor);

    //Controllers
    AddBoolean('Controllers.Enable', False, @ExtensionControllers_GetEnable, @ExtensionControllers_SetEnable, 'On', 'Off');
    AddInteger('Controllers.ScanDelay', 50, @ExtensionControllers_GetScanDelay, @ExtensionControllers_SetScanDelay, 0, 1000);
    AddBoolean('Controllers.AutoScan', False, @ExtensionControllers_GetAutoScan, @ExtensionControllers_SetAutoScan, 'On', 'Off');
    AddInteger('Controllers.AutoScanDelay', 50, @ExtensionControllers_GetAutoScanDelay, @ExtensionControllers_SetAutoScanDelay, 1000, 10000);
    AddInteger('Controllers.Count', 0, @ExtensionControllers_GetCount, nil);

    if SGE.ExtSound <> nil then
      begin
      //Sound
      AddSingle('Sound.Volume', 0.5, @ExtensionSound_GetVolume, @ExtensionSound_SetVolume, 0, 1);

      //Music
      AddSingle('Music.Volume', 0.5, @ExtensionMusic_GetVolume, @ExtensionMusic_SetVolume, 0, 1);
      AddInteger('Music.FadeTime', 3000, @ExtensionMusic_GetFadeTime, @ExtensionMusic_SetFadeTime, 0, $FF);
      AddEnum('Music.ChangeMode', 'Random, Forward, Backward', ',', 0, @ExtensionMusic_GetChangeMode, @ExtensionMusic_SetChangeMode);
      AddEnum('Music.RepeatMode', 'None, Track, List', ',', 2, @ExtensionMusic_GetRepeatMode, @ExtensionMusic_SetRepeatMode);
      AddString('Music.Group', '', @ExtensionMusic_GetGroup, @ExtensionMusic_SetGroup);

      //Track
      AddString('TrackList.Current', '', @ExtensionMusic_TrackList_GetCurrent);
      end;
    end;
end;



end.

