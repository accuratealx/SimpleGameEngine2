{
Пакет             Simple Game Engine 2
Файл              sgeGUIPropertyBorder.pas
Версия            1.0
Создан            16.07.2022
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          GUI: Свойство: Рамка
}
{$Include Defines.inc}

unit sgeGUIPropertyBorder;

{$mode ObjFPC}{$H+}

interface

uses
  sgeSimpleParameters,
  sgeGUIProperty, sgeGUIPropertyColor, sgeGUIPropertyStipple;


type
  TsgeGUIPropertyBorder = class(TsgeGUIProperty)
  private
    FStipple: TsgeGUIPropertyStippleExt;                            //Штриховка
    FColor: TsgeGUIPropertyColorExt;                                //Цвет линии
    FWidth: Integer;                                                //Ширина линии

    procedure SetWidth(AWidth: Integer);

    function  GetStipple: TsgeGUIPropertyStipple;
    function  GetColor: TsgeGUIPropertyColor;
  public
    constructor Create(AOwner: TObject); override;
    destructor  Destroy; override;

    property Stipple: TsgeGUIPropertyStipple read GetStipple;
    property Color: TsgeGUIPropertyColor read GetColor;
    property Width: Integer read FWidth write SetWidth;
  end;


  TsgeGUIPropertyBorderExt = class(TsgeGUIPropertyBorder)
  public
    procedure LoadParameters(Parameters: TsgeSimpleParameters; Prefix: String = '');
  end;


implementation

uses
  sgeGUIUtils;


procedure TsgeGUIPropertyBorder.SetWidth(AWidth: Integer);
begin
  FWidth := AWidth;
  UpdateParent;
end;


function TsgeGUIPropertyBorder.GetStipple: TsgeGUIPropertyStipple;
begin
  Result := FStipple;
end;


function TsgeGUIPropertyBorder.GetColor: TsgeGUIPropertyColor;
begin
  Result := FColor;
end;


constructor TsgeGUIPropertyBorder.Create(AOwner: TObject);
begin
  inherited Create(AOwner);

  FStipple := TsgeGUIPropertyStippleExt.Create(AOwner);
  FColor := TsgeGUIPropertyColorExt.Create(AOwner);

  FWidth := 1;
end;


destructor TsgeGUIPropertyBorder.Destroy;
begin
  FColor.Free;
  FStipple.Free;

  inherited Destroy;
end;


procedure TsgeGUIPropertyBorderExt.LoadParameters(Parameters: TsgeSimpleParameters; Prefix: String);
begin
  //Color
  FColor.LoadParameters(Parameters, Prefix);

  //Stipple
  FStipple.LoadParameters(Parameters, Prefix + 'Stipple.');

  //Width
  sgeGUISetValue(Parameters, Prefix + 'Width', FWidth, 1);
end;


end.

