{
Пакет             Simple Game Engine 2
Файл              sgeGUIPropertyStipple.pas
Версия            1.0
Создан            16.07.2022
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          GUI: Свойство: Штриховка
}
{$Include Defines.inc}

unit sgeGUIPropertyStipple;

{$mode ObjFPC}{$H+}

interface

uses
  sgeSimpleParameters,
  sgeGraphic,
  sgeGUIProperty;


type
  TsgeGUIPropertyStipple = class(TsgeGUIProperty)
  private
    FMode: TsgeGraphicLineStipple;
    FScale: Integer;

    procedure SetMode(AMode: TsgeGraphicLineStipple);
    procedure SetScale(AScale: Integer);
  public
    constructor Create(AOwner: TObject); override;

    property Mode: TsgeGraphicLineStipple read FMode write SetMode;
    property Scale: Integer read FScale write FScale;
  end;


  TsgeGUIPropertyStippleExt = class(TsgeGUIPropertyStipple)
  public
    procedure LoadParameters(Parameters: TsgeSimpleParameters; Prefix: String = '');
  end;


implementation

uses
  sgeGUIUtils;


procedure TsgeGUIPropertyStipple.SetMode(AMode: TsgeGraphicLineStipple);
begin
  FMode := AMode;
  UpdateParent;
end;


procedure TsgeGUIPropertyStipple.SetScale(AScale: Integer);
begin
  FScale := AScale;
  UpdateParent;
end;


constructor TsgeGUIPropertyStipple.Create(AOwner: TObject);
begin
  inherited Create(AOwner);

  FMode := glsSolid;
  FScale := 1;
end;


procedure TsgeGUIPropertyStippleExt.LoadParameters(Parameters: TsgeSimpleParameters; Prefix: String);
var
  ParamName, s: String;
begin
  //Mode
  ParamName := Prefix + 'Mode';
  if Parameters.Exist[ParamName] then
  begin
    s := LowerCase(Parameters.GetValue(ParamName, ''));
    case s of
      'solid':
        SetMode(glsSolid);
      'dash':
        SetMode(glsDash);
      'narrowdash':
        SetMode(glsNarrowDash);
      'widedash':
        SetMode(glsWideDash);
      'dot':
        SetMode(glsDot);
      'dashdot':
        SetMode(glsDashDot);
      'dashdotdot':
        SetMode(glsDashDotDot);
    end;
  end;

  //Scale
  sgeGUISetValue(Parameters, Prefix + 'Scale', FScale);
end;


end.

