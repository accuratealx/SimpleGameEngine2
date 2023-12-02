{
Пакет             Simple Game Engine 2
Файл              sgeAnimationFrameList.pas
Версия            1.0
Создан            09.06.2023
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Список кадров анимации
}
{$Include Defines.inc}

unit sgeAnimationFrameList;

{$mode ObjFPC}{$H+}

interface

uses
  sgeMemoryStream, sgeAnimationFrame;

type
  //Массив кадров
  TsgeAnimationFrameArray = array of TsgeAnimationFrame;


  TsgeAnimationFrameList = class
  private
    FList: TsgeAnimationFrameArray;
    FCount: Integer;

    function GetItem(Index: Integer): TsgeAnimationFrame;
  public
    constructor Create;
    constructor Create(Stream: TsgeMemoryStream);
    destructor  Destroy; override;

    procedure Clear;
    procedure Add(Frame: TsgeAnimationFrame);

    procedure FromString(Str: String);
    procedure FromMemoryStream(Stream: TsgeMemoryStream);

    property Count: Integer read FCount;
    property Item[Index: Integer]: TsgeAnimationFrame read GetItem;
  end;


implementation

uses
  sgeErrors, sgeSystemUtils, sgeStringList;

const
  _UNITNAME = 'AnimationFrameList';

  Err_EmptyMemoryStream = 'EmptyMemoryStream';
  Err_IndexOutOfBounds = 'IndexOutOfBounds';
  Err_CantLoadFromStream = 'CantLoadFromStream';
  Err_EmptyFrame = 'EmptyFrame';
  Err_CorruptData = 'CorruptData';


function TsgeAnimationFrameList.GetItem(Index: Integer): TsgeAnimationFrame;
begin
  if (Index < 0) or (Index > FCount - 1) then
    raise EsgeException.Create(_UNITNAME, Err_IndexOutOfBounds, sgeIntToStr(Index));

  Result := FList[Index];
end;


constructor TsgeAnimationFrameList.Create;
begin
  FCount := 0;
end;


constructor TsgeAnimationFrameList.Create(Stream: TsgeMemoryStream);
begin
  FromMemoryStream(Stream);
end;


destructor TsgeAnimationFrameList.Destroy;
begin
  Clear;
end;


procedure TsgeAnimationFrameList.Clear;
var
  i: Integer;
begin
  for i := 0 to FCount - 1 do
    FList[i].Free;

  SetLength(FList, 0);
  FCount := 0;
end;


procedure TsgeAnimationFrameList.Add(Frame: TsgeAnimationFrame);
begin
  if not Assigned(Frame) then
    raise EsgeException.Create(_UNITNAME, Err_EmptyFrame);

  //Увеличить массив на 1 и записать ссылку на кадр
  SetLength(FList, FCount + 1);
  FList[FCount] := Frame;
  Inc(FCount);
end;


procedure TsgeAnimationFrameList.FromString(Str: String);
var
  List: TsgeStringList;
  i, Cnt: Integer;
  Frm: TsgeAnimationFrame;
  S: String;
begin
  List := TsgeStringList.Create;
  try
    //Разобрать на части
    List.FromString(Str);

    //Проверить что бы был хотя бы один кадр
    Cnt := List.Count - 1;

    //Очистить список кадров
    Clear;

    //Загрузить кадры
    for i := 0 to Cnt do
    begin
      //Получить строку из списка
      S := sgeTrim(List.Part[i]);

      //Пропуск пустых строк
      if S = '' then
        Continue;

      //Создать кадр
      try
        Frm := TsgeAnimationFrame.Create(s);
      except
        on E: EsgeException do
          raise EsgeException.Create(_UNITNAME, Err_CorruptData, sgeIntToStr(i), E.Message);
      end;

      //Добавить в массив кадр
      Add(Frm);
    end;

  finally
    List.Free;
  end;
end;


procedure TsgeAnimationFrameList.FromMemoryStream(Stream: TsgeMemoryStream);
begin
  if not Assigned(Stream) then
    raise EsgeException.Create(_UNITNAME, Err_EmptyMemoryStream);

  try
    FromString(Stream.ToString);
  except
    on E: EsgeException do
      raise EsgeException.Create(_UNITNAME, Err_CantLoadFromStream, '', E.Message);
  end;
end;



end.

