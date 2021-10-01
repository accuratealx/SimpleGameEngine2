{
Пакет             Simple Game Engine 2
Файл              sgeGUIPropertyScaleXY.pas
Версия            1.0
Создан            30.09.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          GUI: Свойство: Масштаб по X и Y
}
{$Include Defines.inc}

unit sgeGUIPropertyScaleXY;

{$mode objfpc}{$H+}

interface

uses
  sgeTypes,
  sgeGUIProperty, sgeGUIPropertyFloatPoint;


type
  //Способ масштабирования XY
  TsgeGUIPropertyScaleXYMode = (smNormal, smStretch, smFitIn, smFitOut, smUserSize, smUserScale);


  TsgeGUIPropertyScaleXY = class(TsgeGUIProperty)
  private
    FMode: TsgeGUIPropertyScaleXYMode;
    FUserSize: TsgeGUIPropertyFloatPoint;
    FUserScale: TsgeGUIPropertyFloatPoint;

    procedure SetMode(AMode: TsgeGUIPropertyScaleXYMode);
  public
    constructor Create(AOwner: TObject); override;
    destructor  Destroy; override;

    property Mode: TsgeGUIPropertyScaleXYMode read FMode write SetMode;
    property UserSize: TsgeGUIPropertyFloatPoint read FUserSize;
    property UserScale: TsgeGUIPropertyFloatPoint read FUserScale;
  end;


  TsgeGUIPropertyScaleXYExt = class(TsgeGUIPropertyScaleXY)
  public
    function GetSize(ParentWidth, ParentHeight, ElementWidth, ElementHeight: Integer): TsgeIntPoint;
  end;



implementation


procedure TsgeGUIPropertyScaleXY.SetMode(AMode: TsgeGUIPropertyScaleXYMode);
begin
  if FMode = AMode then Exit;

  FMode := AMode;
  UpdateParent;
end;


constructor TsgeGUIPropertyScaleXY.Create(AOwner: TObject);
begin
  inherited Create(AOwner);

  FUserSize := TsgeGUIPropertyFloatPoint.Create(AOwner);

  FUserScale := TsgeGUIPropertyFloatPoint.Create(AOwner);
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



function TsgeGUIPropertyScaleXYExt.GetSize(ParentWidth, ParentHeight, ElementWidth, ElementHeight: Integer): TsgeIntPoint;
begin
  case FMode of
    smNormal    : Result := sgeGetIntPoint(ElementWidth, ElementHeight);
    smStretch   : Result := sgeGetIntPoint(ParentWidth, ParentHeight);
    smFitIn     : Result := sgeFitRectIn(ParentWidth, ParentHeight, ElementWidth, ElementHeight);
    smFitOut    : Result := sgeFitRectOut(ParentWidth, ParentHeight, ElementWidth, ElementHeight);
    smUserSize  : Result := sgeGetIntPoint(Round(FUserSize.X), Round(FUserSize.Y));
    smUserScale : Result := sgeGetIntPoint(Round(ElementWidth * FUserScale.X), Round(ElementHeight * FUserScale.Y));
  end;
end;



end.

