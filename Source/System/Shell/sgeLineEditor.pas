{
Пакет             Simple Game Engine 2
Файл              .pas
Версия            1.0
Создан            15.07.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Однострочный редактор текста
}

{$Include Defines.inc}
unit sgeLineEditor;

{$mode objfpc}{$H+}

interface

uses
  sgeTypes;


type
  TsgeLineEditor = class
  private
    FLine: String;                                            //Строка введённого текста
    FStopChars: String;                                       //Стоповые символы
    FCursorPos: Integer;                                      //Текущее положение курсора
    FSelecting: Boolean;                                      //Режим выделения
    FSelectBeginPos: Integer;                                 //Начальное положение курсора выделения
    FSelectEndPos: Integer;                                   //Конечное положение курсора выделения

    procedure GetSelectionIdxAndSize(out Idx, Size: Integer);
    procedure InsertString(APos: Integer; Str: String);
    function  IsStopChar(Chr: Char): Boolean;
    procedure SetLine(ALine: String);
    function  GetSelectCount: Integer;
    procedure SetCursorPos(APos: Integer);
    procedure SetSelectBeginPos(APos: Integer);
    procedure SetSelectEndPos(APos: Integer);
    function  GetLeftStopCharIndex: Integer;
    function  GetRightStopCharIndex: Integer;
    procedure CursorToBegin;
    procedure CursorToEnd;
    procedure CursorToRight;
    procedure CursorToLeft;
    procedure CursorToRightStopChar;
    procedure CursorToLeftStopChar;
    procedure DeleteSymbolRight;
    procedure DeleteSymbolLeft;
    procedure DeleteSymbolRightToStopChar;
    procedure DeleteSymbolLeftToStopChar;
    procedure ClipboardCopy;
    procedure ClipboardPaste;
    procedure ClipboardCut;
  public
    constructor Create;

    procedure SelectAll;
    procedure ClearSelection;
    procedure DeleteSelection;
    function  GetTextBeforePos(APos: Integer): String;
    procedure ProcessChar(Chr: Char; KeyboardButtons: TsgeKeyboardButtons);
    procedure ProcessKey(Key: Byte; KeyboardButtons: TsgeKeyboardButtons);

    property CursorPos: Integer read FCursorPos write SetCursorPos;
    property SelectBeginPos: Integer read FSelectBeginPos write SetSelectBeginPos;
    property SelectEndPos: Integer read FSelectEndPos write SetSelectEndPos;
    property SelectCount: Integer read GetSelectCount;
    property StopChars: String read FStopChars write FStopChars;
    property Line: String read FLine write SetLine;
  end;


implementation

uses
  sgeKeys, sgeOSPlatform;


procedure TsgeLineEditor.GetSelectionIdxAndSize(out Idx, Size: Integer);
begin
  if SelectEndPos > FSelectBeginPos then Idx := FSelectBeginPos else Idx := FSelectEndPos;
  Size := GetSelectCount;
end;


procedure TsgeLineEditor.InsertString(APos: Integer; Str: String);
var
  c: Integer;
begin
  c := Length(FLine);
  if APos < 0 then APos := 0;
  if APos > c then APos := c;
  Insert(Str, FLine, APos + 1);
  FCursorPos := APos + Length(Str);
end;


function TsgeLineEditor.IsStopChar(Chr: Char): Boolean;
var
  i, c: Integer;
begin
  Result := False;
  c := Length(FStopChars);
  for i := 1 to c do
    if Chr = FStopChars[i] then
      begin
      Result := True;
      Break;
      end;
end;


procedure TsgeLineEditor.SetLine(ALine: String);
begin
  FLine := ALine;
  FCursorPos := Length(FLine);
  ClearSelection;
end;


function TsgeLineEditor.GetSelectCount: Integer;
begin
  Result := Abs(FSelectBeginPos - FSelectEndPos);
end;


procedure TsgeLineEditor.SetCursorPos(APos: Integer);
var
  c: Integer;
begin
  c := Length(FLine);
  if APos < 0 then APos := 0;
  if APos > c then APos := c;
  FCursorPos := APos;
end;


procedure TsgeLineEditor.SetSelectBeginPos(APos: Integer);
var
  c: Integer;
begin
  c := Length(FLine);
  if APos < 0 then APos := 0;
  if APos > c then APos := c;
  FSelectBeginPos := APos;
end;


procedure TsgeLineEditor.SetSelectEndPos(APos: Integer);
var
  c: Integer;
begin
  c := Length(FLine);
  if APos < 0 then APos := 0;
  if APos > c then APos := c;
  FSelectEndPos := APos;
end;


function TsgeLineEditor.GetLeftStopCharIndex: Integer;
var
  i: Integer;
  chr1, chr2: Char;
begin
  Result := 0;
  if FCursorPos < 1 then Exit;

  for i := FCursorPos - 1 downto 2 do
    begin
    chr1 := FLine[i];
    chr2 := FLine[i - 1];
    if IsStopChar(chr1) and (chr1 <> chr2) then
      begin
      Result := i;
      Break;
      end;
    end;
end;


function TsgeLineEditor.GetRightStopCharIndex: Integer;
var
  i, c: Integer;
  chr1, chr2: Char;
begin
  c := Length(FLine);
  Result := c;
  Dec(c);
  if FCursorPos > c then Exit;

  for i := FCursorPos + 2 to c do
    begin
    chr1 := FLine[i];
    chr2 := FLine[i + 1];
    if IsStopChar(chr1) and (chr1 <> chr2) then
      begin
      Result := i - 1;
      Break;
      end;
    end;
end;


procedure TsgeLineEditor.CursorToBegin;
begin
  FCursorPos := 0;
  if FSelecting then FSelectEndPos := FCursorPos else ClearSelection;
end;


procedure TsgeLineEditor.CursorToEnd;
begin
  FCursorPos := Length(FLine);
  if FSelecting then FSelectEndPos := FCursorPos else ClearSelection;
end;


procedure TsgeLineEditor.CursorToRight;
var
  c: Integer;
begin
  c := Length(FLine);
  Inc(FCursorPos);
  if FCursorPos > c then FCursorPos := c;
  if FSelecting then FSelectEndPos := FCursorPos else ClearSelection;
end;


procedure TsgeLineEditor.CursorToLeft;
begin
  Dec(FCursorPos);
  if FCursorPos < 0 then FCursorPos := 0;
  if FSelecting then FSelectEndPos := FCursorPos else ClearSelection;
end;


procedure TsgeLineEditor.CursorToRightStopChar;
begin
  FCursorPos := GetRightStopCharIndex;
  if FSelecting then FSelectEndPos := FCursorPos else ClearSelection;
end;


procedure TsgeLineEditor.CursorToLeftStopChar;
begin
  FCursorPos := GetLeftStopCharIndex;
  if FSelecting then FSelectEndPos := FCursorPos else ClearSelection;
end;


procedure TsgeLineEditor.DeleteSymbolRight;
begin
  if FCursorPos > Length(FLine) then Exit;

  if not FSelecting and (SelectCount <> 0) then
    begin
    DeleteSelection;
    ClearSelection;
    Exit;
    end;

  Delete(FLine, FCursorPos + 1, 1);
end;


procedure TsgeLineEditor.DeleteSymbolLeft;
begin
  if FCursorPos < 1 then Exit;

  if not FSelecting and (SelectCount <> 0) then
    begin
    DeleteSelection;
    ClearSelection;
    Exit;
    end;

  Delete(FLine, FCursorPos, 1);
  Dec(FCursorPos);
end;


procedure TsgeLineEditor.DeleteSymbolRightToStopChar;
begin
  if not FSelecting and (SelectCount <> 0) then
    begin
    DeleteSelection;
    ClearSelection;
    Exit;
    end;

  Delete(FLine, FCursorPos + 1, GetRightStopCharIndex - FCursorPos);
end;


procedure TsgeLineEditor.DeleteSymbolLeftToStopChar;
var
  Idx: Integer;
begin
  if not FSelecting and (SelectCount <> 0) then
    begin
    DeleteSelection;
    ClearSelection;
    Exit;
    end;

  Idx := GetLeftStopCharIndex;
  Delete(FLine, Idx + 1, FCursorPos - Idx);
  FCursorPos := Idx;
end;


procedure TsgeLineEditor.ClipboardCopy;
var
  Idx, Count: Integer;
  s: String;
begin
  if SelectCount = 0 then Exit;
  GetSelectionIdxAndSize(Idx, Count);
  s := Copy(FLine, Idx + 1, Count);
  sgeCopyToClipboard(s);
end;


procedure TsgeLineEditor.ClipboardPaste;
var
  Idx: Integer;
  s: String;
begin
  DeleteSelection;
  Idx := sgeCopyFromClipboard(s);
  if Idx <> 0 then Exit;
  InsertString(FCursorPos, s);
  ClearSelection;
end;


procedure TsgeLineEditor.ClipboardCut;
var
  Idx, Count: Integer;
  s: String;
begin
  if SelectCount = 0 then Exit;
  GetSelectionIdxAndSize(Idx, Count);
  s := Copy(FLine, Idx + 1, Count);
  DeleteSelection;
  sgeCopyToClipboard(s);
end;


constructor TsgeLineEditor.Create;
begin
  FStopChars := ' .:=/\,;';
end;


procedure TsgeLineEditor.SelectAll;
begin
  FSelectBeginPos := 0;
  FSelectEndPos := Length(FLine);
  FCursorPos := FSelectEndPos;
end;


procedure TsgeLineEditor.ClearSelection;
begin
  FSelectBeginPos := FCursorPos;
  FSelectEndPos := FCursorPos;
end;


procedure TsgeLineEditor.DeleteSelection;
var
  Idx, Count: Integer;
begin
  if SelectCount = 0 then Exit;
  GetSelectionIdxAndSize(Idx, Count);
  Delete(FLine, Idx + 1, Count);
  FCursorPos := Idx;
  ClearSelection;
end;


function TsgeLineEditor.GetTextBeforePos(APos: Integer): String;
var
  c: Integer;
begin
  c := Length(FLine);
  if APos < 0 then APos := 0;
  if APos > c then APos := c;
  Result := Copy(FLine, 1, APos);
end;


procedure TsgeLineEditor.ProcessChar(Chr: Char; KeyboardButtons: TsgeKeyboardButtons);
begin
  if kbCtrl in KeyboardButtons then Exit;             //Выход, функциональные клавиши
  if not FSelecting then DeleteSelection;             //Удалить выделенное перед вводом
  if Chr < #32 then Exit;                             //Выход, если непечатаемые символы
  if kbShift in KeyboardButtons then DeleteSelection; //Кнопка нажимается вместе с Shift, удалить выделенное

  InsertString(FCursorPos, Chr);                      //Вставить символ
end;


procedure TsgeLineEditor.ProcessKey(Key: Byte; KeyboardButtons: TsgeKeyboardButtons);
begin
  //Проверить режим выделения
  if (kbShift in KeyboardButtons) then
    begin
    FSelecting := True;                       //Включить выделение
    if SelectCount = 0 then ClearSelection;   //Если ничего не выделено, поправить положение
    end else FSelecting := False;             //Иначе отключить

  //Обработать кнопку
  case Key of
    keyHome   : CursorToBegin;
    keyEnd    : CursorToEnd;
    keyLeft   : if (kbCtrl in KeyboardButtons) then CursorToLeftStopChar else CursorToLeft;
    keyRight  : if (kbCtrl in KeyboardButtons) then CursorToRightStopChar else CursorToRight;
    keyDelete : if (kbCtrl in KeyboardButtons) then DeleteSymbolRightToStopChar else DeleteSymbolRight;
    keyBack   : if (kbCtrl in KeyboardButtons) then DeleteSymbolLeftToStopChar else DeleteSymbolLeft;
    keyY      : if (kbCtrl in KeyboardButtons) then SetLine('');
    keyA      : if (kbCtrl in KeyboardButtons) then SelectAll;
    keyX      : if (kbCtrl in KeyboardButtons) then ClipboardCut;
    keyC      : if (kbCtrl in KeyboardButtons) then ClipboardCopy;
    keyV      : if (kbCtrl in KeyboardButtons) then ClipboardPaste;
  end;
end;



end.

