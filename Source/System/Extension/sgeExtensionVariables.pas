{
Пакет             Simple Game Engine 2
Файл              sgeExtensionVariables.pas
Версия            1.5
Создан            20.07.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Класс расширения: Переменные
}
{$Include Defines.inc}

unit sgeExtensionVariables;

{$mode objfpc}{$H+}
{$ModeSwitch duplicatelocals+}

interface

uses
  sgeExtensionBase, sgeVariableList, sgeGraphicColor,
  sgeVariableIntegerNormal, sgeVariableIntegerClass, sgeVariableIntegerProc,
  sgeVariableSingleNormal, sgeVariableSingleClass, sgeVariableSingleProc,
  sgeVariableStringNormal, sgeVariableStringClass, sgeVariableStringProc,
  sgeVariableBooleanNormal, sgeVariableBooleanClass, sgeVariableBooleanProc,
  sgeVariableColorNormal, sgeVariableColorClass, sgeVariableColorProc;


const
  Extension_Variables = 'Variables';


type
  TsgeExtensionVariables = class(TsgeExtensionBase)
  private
    FVariableList: TsgeVariableList;

    procedure CheckVariableExist(VarName: ShortString);
  protected
    class function GetName: String; override;

  public
    constructor Create(ObjectList: TObject); override;
    destructor  Destroy; override;

    //Добавление новых переменных
    function AddInteger(Name: ShortString; Value: Integer; DefValue: Integer; ReadOnly: Boolean = False; MinValue: Integer = -MaxInt; MaxValue: Integer = MaxInt): TsgeVariableIntegerNormal;
    function AddInteger(Name: ShortString; DefValue: Integer; Setter: TsgeVariableIntegerClassSetter; Getter: TsgeVariableIntegerClassGetter; MinValue: Integer = -MaxInt; MaxValue: Integer = MaxInt): TsgeVariableIntegerClass;
    function AddInteger(Name: ShortString; DefValue: Integer; Setter: TsgeVariableIntegerProcSetter; Getter: TsgeVariableIntegerProcGetter; MinValue: Integer = -MaxInt; MaxValue: Integer = MaxInt): TsgeVariableIntegerProc;
    function AddSingle(Name: ShortString; Value: Single; DefValue: Single; ReadOnly: Boolean = False; MinValue: single = 1.5E-45; MaxValue: single = 3.4E38): TsgeVariableSingleNormal;
    function AddSingle(Name: ShortString; DefValue: Single; Getter: TsgeVariableSingleClassGetter; Setter: TsgeVariableSingleClassSetter = nil; MinValue: single = 1.5E-45; MaxValue: single = 3.4E38): TsgeVariableSingleClass;
    function AddSingle(Name: ShortString; DefValue: Single; Getter: TsgeVariableSingleProcGetter; Setter: TsgeVariableSingleProcSetter = nil; MinValue: single = 1.5E-45; MaxValue: single = 3.4E38): TsgeVariableSingleProc;
    function AddString(Name: ShortString; Value: String; DefValue: String; ReadOnly: Boolean = False): TsgeVariableStringNormal;
    function AddString(Name: ShortString; DefValue: String; Getter: TsgeVariableStringClassGetter; Setter: TsgeVariableStringClassSetter = nil): TsgeVariableStringClass;
    function AddString(Name: ShortString; DefValue: String; Getter: TsgeVariableStringProcGetter; Setter: TsgeVariableStringProcSetter = nil): TsgeVariableStringProc;
    function AddBoolean(Name: ShortString; Value: Boolean; DefValue: Boolean; ReadOnly: Boolean = False; TrueStr: ShortString = 'True'; FalseStr: ShortString = 'False'): TsgeVariableBooleanNormal;
    function AddBoolean(Name: ShortString; DefValue: Boolean; Getter: TsgeVariableBooleanClassGetter; Setter: TsgeVariableBooleanClassSetter = nil; TrueStr: ShortString = 'True'; FalseStr: ShortString = 'False'): TsgeVariableBooleanClass;
    function AddBoolean(Name: ShortString; DefValue: Boolean; Getter: TsgeVariableBooleanProcGetter; Setter: TsgeVariableBooleanProcSetter = nil; TrueStr: ShortString = 'True'; FalseStr: ShortString = 'False'): TsgeVariableBooleanProc;
    function AddColor(Name: ShortString; Value: TsgeRGBA; DefValue: TsgeRGBA; ReadOnly: Boolean = False): TsgeVariableColorNormal;
    function AddColor(Name: ShortString; DefValue: TsgeRGBA; Getter: TsgeVariableColorClassGetter; Setter: TsgeVariableColorClassSetter = nil): TsgeVariableColorClass;
    function AddColor(Name: ShortString; DefValue: TsgeRGBA; Getter: TsgeVariableColorProcGetter; Setter: TsgeVariableColorProcSetter = nil): TsgeVariableColorProc;

    //Изменить значение переменной
    procedure SetInteger(Name: ShortString; Value: Integer);
    procedure SetSingle(Name: ShortString; Value: Single);
    procedure SetString(Name: ShortString; Value: String);
    procedure SetBoolean(Name: ShortString; Value: Boolean);
    procedure SetColor(Name: ShortString; Value: TsgeRGBA);

    //Классы
    property Variables: TsgeVariableList read FVariableList;
  end;


implementation

uses
  sgeErrors, sgeSystemUtils, sgeGraphicColorUtils;


const
  _UNITNAME = 'ExtensionVariables';

  Err_VariableExist = 'VariableExist';


procedure TsgeExtensionVariables.CheckVariableExist(VarName: ShortString);
var
  Idx: Integer;
begin
  Idx := FVariableList.IndexOf(VarName);
  if Idx <> -1 then
    raise EsgeException.Create(_UNITNAME, Err_VariableExist, VarName);
end;


class function TsgeExtensionVariables.GetName: String;
begin
  Result := Extension_Variables;
end;


constructor TsgeExtensionVariables.Create(ObjectList: TObject);
begin
  try
    inherited Create(ObjectList);

    FVariableList := TsgeVariableList.Create(True);

  except
    on E: EsgeException do
      raise EsgeException.Create(_UNITNAME, Err_CantCreateExtension, '', E.Message);
  end;
end;


destructor TsgeExtensionVariables.Destroy;
begin
  FVariableList.Free;

  inherited Destroy;
end;


function TsgeExtensionVariables.AddInteger(Name: ShortString; Value: Integer; DefValue: Integer; ReadOnly: Boolean; MinValue: Integer; MaxValue: Integer): TsgeVariableIntegerNormal;
begin
  //Проверить на существование
  CheckVariableExist(Name);

  //Добавить
  Result := TsgeVariableIntegerNormal.Create(Name, Value, DefValue, ReadOnly, MinValue, MaxValue);
  FVariableList.Add(Result);
end;


function TsgeExtensionVariables.AddInteger(Name: ShortString; DefValue: Integer; Setter: TsgeVariableIntegerClassSetter; Getter: TsgeVariableIntegerClassGetter; MinValue: Integer; MaxValue: Integer): TsgeVariableIntegerClass;
begin
  //Проверить на существование
  CheckVariableExist(Name);

  //Добавить
  Result := TsgeVariableIntegerClass.Create(Name, DefValue, Getter, Setter, MinValue, MaxValue);
  FVariableList.Add(Result);
end;


function TsgeExtensionVariables.AddInteger(Name: ShortString; DefValue: Integer; Setter: TsgeVariableIntegerProcSetter; Getter: TsgeVariableIntegerProcGetter; MinValue: Integer; MaxValue: Integer): TsgeVariableIntegerProc;
begin
  //Проверить на существование
  CheckVariableExist(Name);

  //Добавить
  Result := TsgeVariableIntegerProc.Create(Name, DefValue, Getter, Setter, MinValue, MaxValue);
  FVariableList.Add(Result);
end;


function TsgeExtensionVariables.AddSingle(Name: ShortString; Value: Single; DefValue: Single; ReadOnly: Boolean; MinValue: single; MaxValue: single): TsgeVariableSingleNormal;
begin
  //Проверить на существование
  CheckVariableExist(Name);

  //Добавить
  Result := TsgeVariableSingleNormal.Create(Name, Value, DefValue, ReadOnly, MinValue, MaxValue);
  FVariableList.Add(Result);
end;


function TsgeExtensionVariables.AddSingle(Name: ShortString; DefValue: Single; Getter: TsgeVariableSingleClassGetter; Setter: TsgeVariableSingleClassSetter; MinValue: single; MaxValue: single): TsgeVariableSingleClass;
begin
  //Проверить на существование
  CheckVariableExist(Name);

  //Добавить
  Result := TsgeVariableSingleClass.Create(Name, DefValue, Getter, Setter, MinValue, MaxValue);
  FVariableList.Add(Result);
end;


function TsgeExtensionVariables.AddSingle(Name: ShortString; DefValue: Single; Getter: TsgeVariableSingleProcGetter; Setter: TsgeVariableSingleProcSetter; MinValue: single; MaxValue: single): TsgeVariableSingleProc;
begin
  //Проверить на существование
  CheckVariableExist(Name);

  //Добавить
  Result := TsgeVariableSingleProc.Create(Name, DefValue, Getter, Setter, MinValue, MaxValue);
  FVariableList.Add(Result);
end;


function TsgeExtensionVariables.AddString(Name: ShortString; Value: String; DefValue: String; ReadOnly: Boolean): TsgeVariableStringNormal;
begin
  //Проверить на существование
  CheckVariableExist(Name);

  //Добавить
  Result := TsgeVariableStringNormal.Create(Name, Value, DefValue, ReadOnly);
  FVariableList.Add(Result);
end;


function TsgeExtensionVariables.AddString(Name: ShortString; DefValue: String; Getter: TsgeVariableStringClassGetter; Setter: TsgeVariableStringClassSetter): TsgeVariableStringClass;
begin
  //Проверить на существование
  CheckVariableExist(Name);

  //Добавить
  Result := TsgeVariableStringClass.Create(Name, DefValue, Getter, Setter);
  FVariableList.Add(Result);
end;


function TsgeExtensionVariables.AddString(Name: ShortString; DefValue: String; Getter: TsgeVariableStringProcGetter; Setter: TsgeVariableStringProcSetter): TsgeVariableStringProc;
begin
  //Проверить на существование
  CheckVariableExist(Name);

  //Добавить
  Result := TsgeVariableStringProc.Create(Name, DefValue, Getter, Setter);
  FVariableList.Add(Result);
end;


function TsgeExtensionVariables.AddBoolean(Name: ShortString; Value: Boolean; DefValue: Boolean; ReadOnly: Boolean; TrueStr: ShortString; FalseStr: ShortString): TsgeVariableBooleanNormal;
begin
  //Проверить на существование
  CheckVariableExist(Name);

  //Добавить
  Result := TsgeVariableBooleanNormal.Create(Name, Value, DefValue, ReadOnly, TrueStr, FalseStr);
  FVariableList.Add(Result);
end;


function TsgeExtensionVariables.AddBoolean(Name: ShortString; DefValue: Boolean; Getter: TsgeVariableBooleanClassGetter; Setter: TsgeVariableBooleanClassSetter; TrueStr: ShortString; FalseStr: ShortString): TsgeVariableBooleanClass;
begin
  //Проверить на существование
  CheckVariableExist(Name);

  //Добавить
  Result := TsgeVariableBooleanClass.Create(Name, DefValue, Getter, Setter, TrueStr, FalseStr);
  FVariableList.Add(Result);
end;


function TsgeExtensionVariables.AddBoolean(Name: ShortString; DefValue: Boolean; Getter: TsgeVariableBooleanProcGetter; Setter: TsgeVariableBooleanProcSetter; TrueStr: ShortString; FalseStr: ShortString): TsgeVariableBooleanProc;
begin
  //Проверить на существование
  CheckVariableExist(Name);

  //Добавить
  Result := TsgeVariableBooleanProc.Create(Name, DefValue, Getter, Setter, TrueStr, FalseStr);
  FVariableList.Add(Result);
end;


function TsgeExtensionVariables.AddColor(Name: ShortString; Value: TsgeRGBA; DefValue: TsgeRGBA; ReadOnly: Boolean): TsgeVariableColorNormal;
begin
  //Проверить на существование
  CheckVariableExist(Name);

  //Добавить
  Result := TsgeVariableColorNormal.Create(Name, Value, DefValue, ReadOnly);
  FVariableList.Add(Result);
end;


function TsgeExtensionVariables.AddColor(Name: ShortString; DefValue: TsgeRGBA; Getter: TsgeVariableColorClassGetter; Setter: TsgeVariableColorClassSetter): TsgeVariableColorClass;
begin
  //Проверить на существование
  CheckVariableExist(Name);

  //Добавить
  Result := TsgeVariableColorClass.Create(Name, DefValue, Getter, Setter);
  FVariableList.Add(Result);
end;


function TsgeExtensionVariables.AddColor(Name: ShortString; DefValue: TsgeRGBA; Getter: TsgeVariableColorProcGetter; Setter: TsgeVariableColorProcSetter): TsgeVariableColorProc;
begin
  //Проверить на существование
  CheckVariableExist(Name);

  //Добавить
  Result := TsgeVariableColorProc.Create(Name, DefValue, Getter, Setter);
  FVariableList.Add(Result);
end;


procedure TsgeExtensionVariables.SetInteger(Name: ShortString; Value: Integer);
var
  Idx: Integer;
begin
  Idx := FVariableList.IndexOf(Name);
  if Idx = -1 then AddInteger(Name, Value, 0, False) else FVariableList.Item[Idx].StrValue := sgeIntToStr(Value);
end;


procedure TsgeExtensionVariables.SetSingle(Name: ShortString; Value: Single);
var
  Idx: Integer;
begin
  Idx := FVariableList.IndexOf(Name);
  if Idx = -1 then AddSingle(Name, Value, 0, False) else FVariableList.Item[Idx].StrValue := sgeFloatToStr(Value);
end;


procedure TsgeExtensionVariables.SetString(Name: ShortString; Value: String);
var
  Idx: Integer;
begin
  Idx := FVariableList.IndexOf(Name);
  if Idx = -1 then AddString(Name, Value, '', False) else FVariableList.Item[Idx].StrValue := Value;
end;


procedure TsgeExtensionVariables.SetBoolean(Name: ShortString; Value: Boolean);
var
  Idx: Integer;
begin
  Idx := FVariableList.IndexOf(Name);
  if Idx = -1 then AddBoolean(Name, Value, False, False) else FVariableList.Item[Idx].StrValue := sgeBoolToStr(Value);
end;


procedure TsgeExtensionVariables.SetColor(Name: ShortString; Value: TsgeRGBA);
var
  Idx: Integer;
begin
  Idx := FVariableList.IndexOf(Name);
  if Idx = -1 then AddColor(Name, Value, sgeGetRGBA(0, 0, 0, 1), False) else FVariableList.Item[Idx].StrValue := sgeRGBAToString(Value);
end;



end.


