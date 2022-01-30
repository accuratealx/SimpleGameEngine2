{
Пакет             Simple Game Engine 2
Файл              sgeKeyCommandKeyboard.pas
Версия            1.1
Создан            03.08.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Команды на кнопках: Клавиатура
}
{$Include Defines.inc}

unit sgeKeyCommandKeyboard;

{$mode objfpc}{$H+}

interface

uses
  sgeKeyCommandKeyboardActionList;


type
  TsgeKeyCommandKeyboard = class
  const
    MAX_BUTTONS = $FF;

  private
    FList: array[0..MAX_BUTTONS] of TsgeKeyCommandKeyboardActionList;

    function GetCount: Byte;
    function GetKey(Index: Byte): TsgeKeyCommandKeyboardActionList;
    function GetNamedKey(Name: ShortString): TsgeKeyCommandKeyboardActionList;
  public
    constructor Create;
    destructor  Destroy; override;

    function  IndexOf(Name: ShortString): Integer;
    function  GetName(Index: Byte): ShortString;

    procedure Clear;
    procedure Delete(Index: Byte);
    procedure Delete(Name: ShortString);

    property Count: Byte read GetCount;
    property Key[Index: Byte]: TsgeKeyCommandKeyboardActionList read GetKey;
    property NamedKey[Name: ShortString]: TsgeKeyCommandKeyboardActionList read GetNamedKey;
  end;



implementation

uses
  sgeErrors, sgeKeyCommandTypes;

const
  //Имена кнопок клавиатуры
  KeyNames: array[0..$FF] of ShortString = (
    '',             //0
    '',             //1
    '',             //2
    '',             //3
    '',             //4
    '',             //5
    '',             //6
    '',             //7
    'Back',         //8
    'Tab',          //9
    '',             //10
    '',             //11
    '',             //12
    'Enter',        //13
    '',             //14
    '',             //15
    'Shift',        //16
    'Control',      //17
    'Menu',         //18
    'Pause',        //19
    'CapsLock',     //20
    '',             //21
    '',             //22
    '',             //23
    '',             //24
    '',             //25
    '',             //26
    'Escape',       //27
    '',             //28
    '',             //29
    '',             //30
    '',             //31
    'Space',        //32
    'PageUp',       //33
    'PageDown',     //34
    'End',          //35
    'Home',         //36
    'Left',         //37
    'Up',           //38
    'Right',        //39
    'Down',         //40
    '',             //41
    '',             //42
    '',             //43
    '',             //44
    'Insert',       //45
    'Delete',       //46
    '',             //47
    '0',            //48
    '1',            //49
    '2',            //50
    '3',            //51
    '4',            //52
    '5',            //53
    '6',            //54
    '7',            //55
    '8',            //56
    '9',            //57
    '',             //58
    '',             //59
    '',             //60
    '',             //61
    '',             //62
    '',             //63
    '',             //64
    'A',            //65
    'B',            //66
    'C',            //67
    'D',            //68
    'E',            //69
    'F',            //70
    'G',            //71
    'H',            //72
    'I',            //73
    'J',            //74
    'K',            //75
    'L',            //76
    'M',            //77
    'N',            //78
    'O',            //79
    'P',            //80
    'Q',            //81
    'R',            //82
    'S',            //83
    'T',            //84
    'U',            //85
    'V',            //86
    'W',            //87
    'X',            //88
    'Y',            //89
    'Z',            //90
    '',             //91
    '',             //92
    '',             //93
    '',             //94
    '',             //95
    'Num0',         //96
    'Num1',         //97
    'Num2',         //98
    'Num3',         //99
    'Num4',         //100
    'Num5',         //101
    'Num6',         //102
    'Num7',         //103
    'Num8',         //104
    'Num9',         //105
    'NumStar',      //106   Num *
    'NumPlus',      //107   Num +
    '',             //108
    'NumMinus',     //109   Num -
    'NumDel',       //110   Num Del
    'NumDivide',    //111   Num /
    'F1',           //112
    'F2',           //113
    'F3',           //114
    'F4',           //115
    'F5',           //116
    'F6',           //117
    'F7',           //118
    'F8',           //119
    'F9',           //120
    'F10',          //121
    'F11',          //122
    'F12',          //123
    'F13',          //124
    'F14',          //125
    'F15',          //126
    'F16',          //127
    'F17',          //128
    'F18',          //129
    'F19',          //130
    'F20',          //131
    'F21',          //132
    'F22',          //133
    'F23',          //134
    'F24',          //135
    '',             //136
    '',             //137
    '',             //138
    '',             //139
    '',             //140
    '',             //141
    '',             //142
    '',             //143
    'NumLock',      //144
    'ScrollLock',   //145
    '',             //146
    '',             //147
    '',             //148
    '',             //149
    '',             //150
    '',             //151
    '',             //152
    '',             //153
    '',             //154
    '',             //155
    '',             //156
    '',             //157
    '',             //158
    '',             //159
    '',             //160
    '',             //161
    '',             //162
    '',             //163
    '',             //164
    '',             //165
    '',             //166
    '',             //167
    '',             //168
    '',             //169
    '',             //170
    '',             //171
    '',             //172
    '',             //173
    '',             //174
    '',             //175
    '',             //176
    '',             //177
    '',             //178
    '',             //179
    '',             //180
    '',             //181
    '',             //182
    '',             //183
    '',             //184
    '',             //185
    'Semicolon',    //186   ;
    'Plus',         //187   +
    'Comma',        //188   ,
    'Minus',        //189   -
    'Dot',          //190   .
    'Divide',       //191   /
    'Tilde',        //192   ~
    '',             //193
    '',             //194
    '',             //195
    '',             //196
    '',             //197
    '',             //198
    '',             //199
    '',             //200
    '',             //201
    '',             //202
    '',             //203
    '',             //204
    '',             //205
    '',             //206
    '',             //207
    '',             //208
    '',             //209
    '',             //210
    '',             //211
    '',             //212
    '',             //213
    '',             //214
    '',             //215
    '',             //216
    '',             //217
    '',             //218
    'OpenBracket',  //219   [
    'Slash',        //220   \
    'CloseBracket', //221   ]
    'Quote',        //222   "
    '',             //223
    '',             //224
    '',             //225
    '',             //226
    '',             //227
    '',             //228
    '',             //229
    '',             //230
    '',             //231
    '',             //232
    '',             //233
    '',             //234
    '',             //235
    '',             //236
    '',             //237
    '',             //238
    '',             //239
    '',             //240
    '',             //241
    '',             //242
    '',             //243
    '',             //244
    '',             //245
    '',             //246
    '',             //247
    '',             //248
    '',             //249
    '',             //250
    '',             //251
    '',             //252
    '',             //253
    '',             //254
    ''              //255
  );

  _UNITNAME = 'KeyCommandKeyboard';


function TsgeKeyCommandKeyboard.GetCount: Byte;
begin
  Result := MAX_BUTTONS;
end;


function TsgeKeyCommandKeyboard.GetKey(Index: Byte): TsgeKeyCommandKeyboardActionList;
begin
  Result := FList[Index];
end;


function TsgeKeyCommandKeyboard.GetNamedKey(Name: ShortString): TsgeKeyCommandKeyboardActionList;
var
  Idx: Integer;
begin
  Idx := IndexOf(Name);
  if Idx = -1 then
    raise EsgeException.Create(_UNITNAME, Err_KeyNotFound, Name);

  Result := FList[Idx];
end;


constructor TsgeKeyCommandKeyboard.Create;
var
  i: Integer;
begin
  for i := 0 to MAX_BUTTONS do
    FList[i] := TsgeKeyCommandKeyboardActionList.Create;
end;


destructor TsgeKeyCommandKeyboard.Destroy;
var
  i: Integer;
begin
  for i := 0 to MAX_BUTTONS do
    FList[i].Free;
end;


function TsgeKeyCommandKeyboard.IndexOf(Name: ShortString): Integer;
var
  i: Byte;
begin
  Result := -1;

  Name := LowerCase(Name);
  for i := 0 to MAX_BUTTONS do
    if Name = LowerCase(KeyNames[i]) then
      begin
      Result := i;
      Break;
      end;
end;


function TsgeKeyCommandKeyboard.GetName(Index: Byte): ShortString;
begin
  Result := KeyNames[Index];
end;


procedure TsgeKeyCommandKeyboard.Clear;
var
  i: Integer;
begin
  for i := 0 to MAX_BUTTONS do
    Delete(i);
end;


procedure TsgeKeyCommandKeyboard.Delete(Index: Byte);
begin
  FList[Index].Clear;
end;


procedure TsgeKeyCommandKeyboard.Delete(Name: ShortString);
var
  Idx: Integer;
begin
  Idx := IndexOf(Name);
  if Idx = -1 then
    raise EsgeException.Create(_UNITNAME, Err_KeyNotFound, Name);

  Delete(Idx);
end;




end.

