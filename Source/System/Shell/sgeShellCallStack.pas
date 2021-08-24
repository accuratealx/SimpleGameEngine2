unit sgeShellCallStack;

{$mode objfpc}{$H+}

interface

uses
  sgeTemplateObjectCollection, sgeShellCallStackItem;


type
  //Шаблон списка
  TsgeShellCallStackTemplate = specialize TsgeTemplateObjectCollection<TsgeShellStackItem>;


  TsgeShellCallStack = class(TsgeShellCallStackTemplate)
  private
  public
    function Add(ScriptName: ShortString; Pos: Integer = 0): TsgeShellStackItem;

    function GetLast: TsgeShellStackItem;
  end;




implementation


function TsgeShellCallStack.Add(ScriptName: ShortString; Pos: Integer): TsgeShellStackItem;
begin
  Result := TsgeShellStackItem.Create(ScriptName, Pos);
  inherited Add(Result);
end;


function TsgeShellCallStack.GetLast: TsgeShellStackItem;
begin
  Result := nil;

  if FCount > 0 then Result := FList[FCount - 1];
end;



end.

