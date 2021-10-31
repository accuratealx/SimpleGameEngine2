{
Пакет             Simple Game Engine 2
Файл              sgeGUIPropertyDrawMethod.pas
Версия            1.1
Создан            01.10.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          GUI: Свойство: режим вывода спрайта
}
{$Include Defines.inc}

unit sgeGUIPropertyDrawMethod;

{$mode objfpc}{$H+}

interface

uses
  sgeSimpleParameters,
  sgeGUIProperty, sgeGUIPropertySegmentOffset;


type
  //Режим вывода спрайта (Одним вызовом, как кнопку)
  TsgeGUIPropertyDrawMethodMode = (dmmNormal, dmmSegment);


  TsgeGUIPropertyDrawMethod = class(TsgeGUIProperty)
  private
    FMode: TsgeGUIPropertyDrawMethodMode;
    FOffset: TsgeGUIPropertySegmentOffsetExt;

    procedure SetMode(AMode: TsgeGUIPropertyDrawMethodMode);

    function GetOffset: TsgeGUIPropertySegmentOffset;
  public
    constructor Create(AOwner: TObject); override;
    destructor  Destroy; override;

    property Mode: TsgeGUIPropertyDrawMethodMode read FMode write SetMode;
    property Offset: TsgeGUIPropertySegmentOffset read GetOffset;
  end;


  TsgeGUIPropertyDrawMethodExt = class(TsgeGUIPropertyDrawMethod)
  public
    procedure LoadParameters(Parameters: TsgeSimpleParameters; Prefix: String = '');
  end;


implementation


procedure TsgeGUIPropertyDrawMethod.SetMode(AMode: TsgeGUIPropertyDrawMethodMode);
begin
  if FMode = AMode then Exit;

  FMode := AMode;
  UpdateParent;
end;


function TsgeGUIPropertyDrawMethod.GetOffset: TsgeGUIPropertySegmentOffset;
begin
  Result := FOffset;
end;


constructor TsgeGUIPropertyDrawMethod.Create(AOwner: TObject);
begin
  inherited Create(AOwner);

  FOffset := TsgeGUIPropertySegmentOffsetExt.Create(AOwner);
end;


destructor TsgeGUIPropertyDrawMethod.Destroy;
begin
  FOffset.Free;

  inherited Destroy;
end;


procedure TsgeGUIPropertyDrawMethodExt.LoadParameters(Parameters: TsgeSimpleParameters; Prefix: String);
var
  ParamName, s: String;
begin
  //Mode
  ParamName := Prefix + 'Mode';
  if Parameters.Exist[ParamName] then
    begin
    s := LowerCase(Parameters.GetValue(ParamName, ''));
    case s of
      'normal'  : FMode := dmmNormal;
      'segment' : FMode := dmmSegment;
    end;
    end;

  //FOffset
  FOffset.LoadParameters(Parameters, Prefix + 'Offset.');
end;


end.

