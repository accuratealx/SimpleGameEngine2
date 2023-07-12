{
Пакет             Simple Game Engine 2
Файл              sgeEventGraphic.pas
Версия            1.3
Создан            06.05.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Классы событий: Графика
}
{$Include Defines.inc}

unit sgeEventGraphic;

{$mode objfpc}{$H+}
{$ModeSwitch duplicatelocals+}

interface

uses
  sgeTypes, sgeMemoryStream, sgeEventBase,
  sgeGraphicColor,
  sgeDisplayElement, sgeDisplayLayer,
  sgeGraphicOpenGLDrawObjectFadeItem;


const
  //Шейдеры
  Event_Graphic_ShaderAdd   = 'Graphic.ShaderAdd';

  //Затемнение
  Event_Graphic_Fade        = 'Graphic.Fade';
  Event_Graphic_FadeNew     = 'Graphic.FadeNew';

  //Слои
  Event_Graphic_LayerAdd    = 'Graphic.LayerAdd';
  Event_Graphic_LayerUpdate = 'Graphic.LayerUpdate';
  Event_Graphic_LayerDelete = 'Graphic.LayerDelete';

  //Элементы
  Event_Graphic_ItemAdd     = 'Graphic.ItemAdd';
  Event_Graphic_ItemUpdate  = 'Graphic.ItemUpdate';
  Event_Graphic_ItemDelete  = 'Graphic.ItemDelete';
  Event_Graphic_ItemVisible = 'Graphic.ItemVisible';


type
  //Шейдеры
  TsgeEventGraphicShaderAdd = class(TsgeEventBase)
  private
    FShaderName: String;
    FShaderStream: TsgeMemoryStream;
  public
    constructor Create(Name: ShortString; ShaderName: String; Stream: TsgeMemoryStream);

    function Copy: TsgeEventBase; override;

    property ShaderName: String read FShaderName;
    property ShaderStream: TsgeMemoryStream read FShaderStream;
  end;


  //Затемнение
  TsgeEventGraphicFade = class(TsgeEventBase)
  private
    FPassedTime: TsgePassedTime;
    FID: Integer;
  public
    constructor Create(Name: ShortString; PassedTime: TsgePassedTime; ID: Integer);

    function Copy: TsgeEventBase; override;

    property PassedTime: TsgePassedTime read FPassedTime;
    property ID: Integer read FID;
  end;


  TsgeEventGraphicFadeNew = class(TsgeEventBase)
  private
    FMode: TsgeFadeMode;
    FColor: TsgeColor;
    FTime: Cardinal;
    FID: Integer;
    FTimeProc: TsgeFadeProc;
  public
    constructor Create(Name: ShortString; Mode: TsgeFadeMode; Color: TsgeColor; Time: Cardinal; ID: Integer; TimeProc: TsgeFadeProc);

    function Copy: TsgeEventBase; override;

    property Mode: TsgeFadeMode read FMode;
    property Color: TsgeColor read FColor;
    property Time: Cardinal read FTime;
    property ID: Integer read FID;
    property TimeProc: TsgeFadeProc read FTimeProc;
  end;


  //Слои
  TsgeEventGraphicLayer = class(TsgeEventBase)
  private
    FUniqueID: Integer;
    FLayer: TsgeDisplayLayer;
  public
    constructor Create(Name: ShortString; UniqueID: Integer; Layer: TsgeDisplayLayer = nil);

    function Copy: TsgeEventBase; override;

    property UniqueID: Integer read FUniqueID;
    property Layer: TsgeDisplayLayer read FLayer;
  end;


  //Элементы
  TsgeEventGraphicElement = class(TsgeEventBase)
  private
    FUniqueID: Integer;
  public
    constructor Create(Name: ShortString; UniqueID: Integer);

    function Copy: TsgeEventBase; override;

    property UniqueID: Integer read FUniqueID;
  end;


  TsgeEventGraphicElementDelete = class(TsgeEventGraphicElement)
  public
    constructor Create(UniqueID: Integer);
  end;


  TsgeEventGraphicElementVisible = class(TsgeEventGraphicElement)
  private
    FVisible: Boolean;
  public
    constructor Create(UniqueID: Integer; Visible: Boolean);

    function Copy: TsgeEventBase; override;

    property Visible: Boolean read FVisible;
  end;


  TsgeEventGraphicElementUpdate = class(TsgeEventGraphicElement)
  private
    FItem: TsgeDisplayElement;
  public
    constructor Create(UniqueID: Integer; Item: TsgeDisplayElement);

    function Copy: TsgeEventBase; override;

    property Item: TsgeDisplayElement read FItem;
  end;


  TsgeEventGraphicElementAdd = class(TsgeEventGraphicElement)
  private
    FLayerName: String;
    FItem: TsgeDisplayElement;
  public
    constructor Create(UniqueID: Integer; Item: TsgeDisplayElement; LayerName: String);

    function Copy: TsgeEventBase; override;

    property LayerName: String read FLayerName;
    property Item: TsgeDisplayElement read FItem;
  end;



implementation


constructor TsgeEventGraphicShaderAdd.Create(Name: ShortString; ShaderName: String; Stream: TsgeMemoryStream);
begin
  inherited Create(Name);

  FShaderName := ShaderName;
  FShaderStream := Stream;
end;


function TsgeEventGraphicShaderAdd.Copy: TsgeEventBase;
begin
  Result := TsgeEventGraphicShaderAdd.Create(FName, FShaderName, FShaderStream);
end;



constructor TsgeEventGraphicFade.Create(Name: ShortString; PassedTime: TsgePassedTime; ID: Integer);
begin
  inherited Create(Name);

  FPassedTime := PassedTime;
  FID := ID;
end;


function TsgeEventGraphicFade.Copy: TsgeEventBase;
begin
  Result := TsgeEventGraphicFade.Create(FName, FPassedTime, FID);
end;



constructor TsgeEventGraphicFadeNew.Create(Name: ShortString; Mode: TsgeFadeMode; Color: TsgeColor; Time: Cardinal; ID: Integer; TimeProc: TsgeFadeProc);
begin
  inherited Create(Name);

  FMode := Mode;
  FColor := Color;
  FTime := Time;
  FID := ID;
  FTimeProc := TimeProc;
end;


function TsgeEventGraphicFadeNew.Copy: TsgeEventBase;
begin
  Result := TsgeEventGraphicFadeNew.Create(FName, FMode, FColor, FTime, FID, FTimeProc);
end;



constructor TsgeEventGraphicLayer.Create(Name: ShortString; UniqueID: Integer; Layer: TsgeDisplayLayer);
begin
  inherited Create(Name);
  FUniqueID := UniqueID;
  FLayer := Layer;
end;


function TsgeEventGraphicLayer.Copy: TsgeEventBase;
begin
  Result := TsgeEventGraphicLayer.Create(FName, FUniqueID, FLayer);
end;



constructor TsgeEventGraphicElement.Create(Name: ShortString; UniqueID: Integer);
begin
  inherited Create(Name);
  FUniqueID := UniqueID;
end;


function TsgeEventGraphicElement.Copy: TsgeEventBase;
begin
  Result := TsgeEventGraphicElement.Create(FName, FUniqueID);
end;


constructor TsgeEventGraphicElementDelete.Create(UniqueID: Integer);
begin
  inherited Create(Event_Graphic_ItemDelete, UniqueID);
end;


constructor TsgeEventGraphicElementVisible.Create(UniqueID: Integer; Visible: Boolean);
begin
  inherited Create(Event_Graphic_ItemVisible, UniqueID);
  FVisible := Visible;
end;


function TsgeEventGraphicElementVisible.Copy: TsgeEventBase;
begin
  Result := TsgeEventGraphicElementVisible.Create(FUniqueID, FVisible);
end;


constructor TsgeEventGraphicElementUpdate.Create(UniqueID: Integer; Item: TsgeDisplayElement);
begin
  inherited Create(Event_Graphic_ItemUpdate, UniqueID);
  FItem := Item;
end;


function TsgeEventGraphicElementUpdate.Copy: TsgeEventBase;
begin
  Result := TsgeEventGraphicElementUpdate.Create(FUniqueID, FItem);
end;


constructor TsgeEventGraphicElementAdd.Create(UniqueID: Integer; Item: TsgeDisplayElement; LayerName: String);
begin
  inherited Create(Event_Graphic_ItemAdd, UniqueID);
  FItem := Item;
  FLayerName := LayerName;
end;


function TsgeEventGraphicElementAdd.Copy: TsgeEventBase;
begin
  Result := TsgeEventGraphicElementAdd.Create(FUniqueID, FItem, FLayerName);
end;



end.

