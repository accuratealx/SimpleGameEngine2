{
Пакет             Simple Game Engine 2
Файл              sgeVariables.pas
Версия            1.0
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
  SimpleGameEngine, sgeGraphicColor;


var
  SGE: TSimpleGameEngine;


{$region Shell}
procedure ExtensionShell_SetEnable(AEnable: Boolean);
begin
  SGE.ExtShell.Enable := AEnable;
end;

function  ExtensionShell_GetEnable: Boolean;
begin
  Result := SGE.ExtShell.Enable;
end;


procedure ExtensionShell_SetBGColor(AColor: TsgeRGBA);
begin
  SGE.ExtShell.BGColor := sgeRGBAToColor(AColor);
end;

function  ExtensionShell_GetBGColor: TsgeRGBA;
begin
  Result := sgeColorToRGBA(SGE.ExtShell.BGColor);
end;


procedure ExtensionShell_SetEditorTextColor(AColor: TsgeRGBA);
begin
  SGE.ExtShell.EditorTextColor := sgeRGBAToColor(AColor);
end;

function  ExtensionShell_GetEditorTextColor: TsgeRGBA;
begin
  Result := sgeColorToRGBA(SGE.ExtShell.EditorTextColor);
end;


procedure ExtensionShell_SetEditorSelectColor(AColor: TsgeRGBA);
begin
  SGE.ExtShell.EditorSelectColor := sgeRGBAToColor(AColor);
end;

function  ExtensionShell_GetEditorSelectColor: TsgeRGBA;
begin
  Result := sgeColorToRGBA(SGE.ExtShell.EditorSelectColor);
end;


procedure ExtensionShell_SetEditorCursorColor(AColor: TsgeRGBA);
begin
  SGE.ExtShell.EditorCursorColor := sgeRGBAToColor(AColor);
end;

function  ExtensionShell_GetEditorCursorColor: TsgeRGBA;
begin
  Result := sgeColorToRGBA(SGE.ExtShell.EditorCursorColor);
end;


procedure ExtensionShell_SetErrorColor(AColor: TsgeRGBA);
begin
  SGE.ExtShell.ErrorColor := sgeRGBAToColor(AColor);
end;

function  ExtensionShell_GetErrorColor: TsgeRGBA;
begin
  Result := sgeColorToRGBA(SGE.ExtShell.ErrorColor);
end;


procedure ExtensionShell_SetTextColor(AColor: TsgeRGBA);
begin
  SGE.ExtShell.TextColor := sgeRGBAToColor(AColor);
end;

function  ExtensionShell_GetTextColor: TsgeRGBA;
begin
  Result := sgeColorToRGBA(SGE.ExtShell.TextColor);
end;


procedure ExtensionShell_SetNoteColor(AColor: TsgeRGBA);
begin
  SGE.ExtShell.NoteColor := sgeRGBAToColor(AColor);
end;

function  ExtensionShell_GetNoteColor: TsgeRGBA;
begin
  Result := sgeColorToRGBA(SGE.ExtShell.NoteColor);
end;
{$EndRegion Shell}





//////////////////////////////////////////////////
procedure sgeVariables_Init(SGEObject: TObject);
begin
  //Запомнить указатель
  SGE := TSimpleGameEngine(SGEObject);

  //Добавить переменные
  with SGE.ExtVariables do
    begin
    AddBoolean('Shell.Enable', False, @ExtensionShell_GetEnable, @ExtensionShell_SetEnable, 'On', 'Off');
    AddColor('Shell.BGColor', sgeGetRGBA(0, 0, 0, 128), @ExtensionShell_GetBGColor, @ExtensionShell_SetBGColor);
    AddColor('Shell.EditorTextColor', sgeGetRGBA(255, 255, 255, 255), @ExtensionShell_GetEditorTextColor, @ExtensionShell_SetEditorTextColor);
    AddColor('Shell.EditorSelectColor', sgeGetRGBA(255, 255, 255, 128), @ExtensionShell_GetEditorSelectColor, @ExtensionShell_SetEditorSelectColor);
    AddColor('Shell.EditorCursorColor', sgeGetRGBA(255, 255, 255, 255), @ExtensionShell_GetEditorCursorColor, @ExtensionShell_SetEditorCursorColor);
    AddColor('Shell.ErrorColor', sgeGetRGBA(255, 0, 0, 255), @ExtensionShell_GetErrorColor, @ExtensionShell_SetErrorColor);
    AddColor('Shell.TextColor', sgeGetRGBA(255, 255, 255, 255), @ExtensionShell_GetTextColor, @ExtensionShell_SetTextColor);
    AddColor('Shell.NoteColor', sgeGetRGBA(255, 255, 255, 128), @ExtensionShell_GetNoteColor, @ExtensionShell_SetNoteColor);
    end;

end;



end.

