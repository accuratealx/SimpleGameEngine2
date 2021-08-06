{
Пакет             Simple Game Engine 2
Файл              sgeExtensionVariables.pas
Версия            1.4
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
  sgeExtensionBase, sgeVariableList,
  sgeVariableInteger, sgeVariableIntegerVirtual, sgeVariableSingle, sgeVariableSingleVirtual,
  sgeVariableString, sgeVariableStringVirtual, sgeVariableBoolean, sgeVariableBooleanVirtual;


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

    procedure Delete(Name: ShortString);

    //Добавление новых переменных
    function AddInteger(Name: ShortString; Value: Integer; DefValue: Integer; ReadOnly: Boolean = False; MinValue: Integer = -MaxInt; MaxValue: Integer = MaxInt): TsgeVariableInteger;
    function AddInteger(Name: ShortString; DefValue: Integer; Setter: TsgeVariableIntegerSetter; Getter: TsgeVariableIntegerGetter; MinValue: Integer = -MaxInt; MaxValue: Integer = MaxInt): TsgeVariableIntegerVirtual;
    function AddSingle(Name: ShortString; Value: Single; DefValue: Single; ReadOnly: Boolean = False; MinValue: single = 1.5E-45; MaxValue: single = 3.4E38): TsgeVariableSingle;
    function AddSingle(Name: ShortString; DefValue: Single; Getter: TsgeVariableSingleGetter; Setter: TsgeVariableSingleSetter = nil; MinValue: single = 1.5E-45; MaxValue: single = 3.4E38): TsgeVariableSingleVirtual;
    function AddString(Name: ShortString; Value: String; DefValue: String; ReadOnly: Boolean = False): TsgeVariableString;
    function AddString(Name: ShortString; DefValue: String; Getter: TsgeVariableStringGetter; Setter: TsgeVariableStringSetter = nil): TsgeVariableStringVirtual;
    function AddBoolean(Name: ShortString; Value: Boolean; DefValue: Boolean; ReadOnly: Boolean = False; TrueStr: ShortString = 'True'; FalseStr: ShortString = 'False'): TsgeVariableBoolean;
    function AddBoolean(Name: ShortString; DefValue: Boolean; Getter: TsgeVariableBooleanGetter; Setter: TsgeVariableBooleanSetter = nil; TrueStr: ShortString = 'True'; FalseStr: ShortString = 'False'): TsgeVariableBooleanVirtual;

    //Изменить значение переменной
    procedure SetInteger(Name: ShortString; Value: Integer);
    procedure SetSingle(Name: ShortString; Value: Single);
    procedure SetString(Name: ShortString; Value: String);
    procedure SetBoolean(Name: ShortString; Value: Boolean);

    //Классы
    property Variables: TsgeVariableList read FVariableList;
  end;


implementation

uses
  sgeErrors, sgeSystemUtils;


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


procedure TsgeExtensionVariables.Delete(Name: ShortString);
begin
  FVariableList.Delete(Name);
end;


function TsgeExtensionVariables.AddInteger(Name: ShortString; Value: Integer; DefValue: Integer; ReadOnly: Boolean; MinValue: Integer; MaxValue: Integer): TsgeVariableInteger;
begin
  //Проверить на существование
  CheckVariableExist(Name);

  //Добавить
  Result := TsgeVariableInteger.Create(Name, Value, DefValue, ReadOnly, MinValue, MaxValue);
  FVariableList.Add(Result);
end;


function TsgeExtensionVariables.AddInteger(Name: ShortString; DefValue: Integer; Setter: TsgeVariableIntegerSetter; Getter: TsgeVariableIntegerGetter; MinValue: Integer; MaxValue: Integer): TsgeVariableIntegerVirtual;
begin
  //Проверить на существование
  CheckVariableExist(Name);

  //Добавить
  Result := TsgeVariableIntegerVirtual.Create(Name, DefValue, Getter, Setter, MinValue, MaxValue);
  FVariableList.Add(Result);
end;


function TsgeExtensionVariables.AddSingle(Name: ShortString; Value: Single; DefValue: Single; ReadOnly: Boolean; MinValue: single; MaxValue: single): TsgeVariableSingle;
begin
  //Проверить на существование
  CheckVariableExist(Name);

  //Добавить
  Result := TsgeVariableSingle.Create(Name, Value, DefValue, ReadOnly, MinValue, MaxValue);
  FVariableList.Add(Result);
end;


function TsgeExtensionVariables.AddSingle(Name: ShortString; DefValue: Single; Getter: TsgeVariableSingleGetter; Setter: TsgeVariableSingleSetter; MinValue: single; MaxValue: single): TsgeVariableSingleVirtual;
begin
  //Проверить на существование
  CheckVariableExist(Name);

  //Добавить
  Result := TsgeVariableSingleVirtual.Create(Name, DefValue, Getter, Setter, MinValue, MaxValue);
  FVariableList.Add(Result);
end;


function TsgeExtensionVariables.AddString(Name: ShortString; Value: String; DefValue: String; ReadOnly: Boolean): TsgeVariableString;
begin
  //Проверить на существование
  CheckVariableExist(Name);

  //Добавить
  Result := TsgeVariableString.Create(Name, Value, DefValue, ReadOnly);
  FVariableList.Add(Result);
end;


function TsgeExtensionVariables.AddString(Name: ShortString; DefValue: String; Getter: TsgeVariableStringGetter; Setter: TsgeVariableStringSetter): TsgeVariableStringVirtual;
begin
  //Проверить на существование
  CheckVariableExist(Name);

  //Добавить
  Result := TsgeVariableStringVirtual.Create(Name, DefValue, Getter, Setter);
  FVariableList.Add(Result);
end;


function TsgeExtensionVariables.AddBoolean(Name: ShortString; Value: Boolean; DefValue: Boolean; ReadOnly: Boolean; TrueStr: ShortString; FalseStr: ShortString): TsgeVariableBoolean;
begin
  //Проверить на существование
  CheckVariableExist(Name);

  //Добавить
  Result := TsgeVariableBoolean.Create(Name, Value, DefValue, ReadOnly, TrueStr, FalseStr);
  FVariableList.Add(Result);
end;


function TsgeExtensionVariables.AddBoolean(Name: ShortString; DefValue: Boolean; Getter: TsgeVariableBooleanGetter; Setter: TsgeVariableBooleanSetter; TrueStr: ShortString; FalseStr: ShortString): TsgeVariableBooleanVirtual;
begin
  //Проверить на существование
  CheckVariableExist(Name);

  //Добавить
  Result := TsgeVariableBooleanVirtual.Create(Name, DefValue, Getter, Setter, TrueStr, FalseStr);
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



end.


