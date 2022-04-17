{
Пакет             Simple Game Engine 2
Файл              sgeGUIPropertyScaleXY.pas
Версия            1.1
Создан            30.09.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          GUI: Свойство: Масштаб по X и Y
}
{$Include Defines.inc}

unit sgeGUIPropertyScaleXY;

{$mode objfpc}{$H+}

interface

uses
  sgeTypes, sgeSimpleParameters,
  sgeGUIProperty, sgeGUIPropertyFloatPoint;


type
  //Способ масштабирования XY
  TsgeGUIPropertyScaleXYMode = (smNormal, smStretch, smFitIn, smFitOut, smUserSize, smUserScale);


  TsgeGUIPropertyScaleXY = class(TsgeGUIProperty)
  private
    FMode: TsgeGUIPropertyScaleXYMode;
    FUserSize: TsgeGUIPropertyFloatPointExt;
    FUserScale: TsgeGUIPropertyFloatPointExt;

    procedure SetMode(AMode: TsgeGUIPropertyScaleXYMode);

    function GetUserSize: TsgeGUIPropertyFloatPoint;
    function GetUserScale: TsgeGUIPropertyFloatPoint;
  public
    constructor Create(AOwner: TObject); override;
    destructor  Destroy; override;

    property Mode: TsgeGUIPropertyScaleXYMode read FMode write SetMode;
    property UserSize: TsgeGUIPropertyFloatPoint read GetUserSize;
    property UserScale: TsgeGUIPropertyFloatPoint read GetUserScale;
  end;


  TsgeGUIPropertyScaleXYExt = class(TsgeGUIPropertyScaleXY)
  public
    procedure LoadParameters(Parameters: TsgeSimpleParameters; Prefix: String = '');
    function GetSize(ParentWidth, ParentHeight, ElementWidth, ElementHeight: Integer): TsgeIntPoint;
  end;



implementation


procedure TsgeGUIPropertyScaleXY.SetMode(AMode: TsgeGUIPropertyScaleXYMode);
begin
  if FMode = AMode then
    Exit;

  FMode := AMode;
  UpdateParent;
end;


function TsgeGUIPropertyScaleXY.GetUserSize: TsgeGUIPropertyFloatPoint;
begin
  Result := FUserSize;
end;


function TsgeGUIPropertyScaleXY.GetUserScale: TsgeGUIPropertyFloatPoint;
begin
  Result := FUserScale;
end;


constructor TsgeGUIPropertyScaleXY.Create(AOwner: TObject);
begin
  inherited Create(AOwner);

  FUserSize := TsgeGUIPropertyFloatPointExt.Create(AOwner);

  FUserScale := TsgeGUIPropertyFloatPointExt.Create(AOwner);
  FUserScale.LockUpdate;
  FUserScale.X := 1;
  FUserScale.Y := 1;
  FUserScale.UnlockUpdate;

  FMode := smNormal;
end;


destructor TsgeGUIPropertyScaleXY.Destroy;
begin
  FUserScale.Free;
  FUserSize.Free;

  inherited Destroy;
end;


procedure TsgeGUIPropertyScaleXYExt.LoadParameters(Parameters: TsgeSimpleParameters; Prefix: String);
var
  ParamName, s: String;
begin
  //Mode
  ParamName := Prefix + 'Mode';
  if Parameters.Exist[ParamName] then
  begin
    s := LowerCase(Parameters.GetValue(ParamName, ''));
    case s of
      'normal':
        FMode := smNormal;

      'stretch':
        FMode := smStretch;

      'fitin':
        FMode := smFitIn;

      'fitout':
        FMode := smFitOut;

      'usersize':
        FMode := smUserSize;

      'userscale':
        FMode := smUserScale;
    end;
  end;

  //UserSize
  FUserSize.LoadParameters(Parameters, Prefix + 'UserSize.');

  //UserScale
  FUserScale.LoadParameters(Parameters, Prefix + 'UserScale.');
end;


function TsgeGUIPropertyScaleXYExt.GetSize(ParentWidth, ParentHeight, ElementWidth, ElementHeight: Integer): TsgeIntPoint;
begin
  case FMode of
    smNormal:
      Result := sgeGetIntPoint(ElementWidth, ElementHeight);

    smStretch:
      Result := sgeGetIntPoint(ParentWidth, ParentHeight);

    smFitIn:
      Result := sgeFitRectIn(ParentWidth, ParentHeight, ElementWidth, ElementHeight);

    smFitOut:
      Result := sgeFitRectOut(ParentWidth, ParentHeight, ElementWidth, ElementHeight);

    smUserSize:
      Result := sgeGetIntPoint(Round(FUserSize.X), Round(FUserSize.Y));

    smUserScale:
      Result := sgeGetIntPoint(Round(ElementWidth * FUserScale.X), Round(ElementHeight * FUserScale.Y));
  end;
end;



end.

