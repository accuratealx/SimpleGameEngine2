unit MainUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, ComCtrls,
  ExtCtrls, FileUtil, LCLType,

  sgePackFileWriter,
  ProgressUnit;

type
  TMainFrm = class(TForm)
    FolderView: TListView;
    FolderTreeImageList: TImageList;
    FolderViewPopupImageList: TImageList;
    FolderTreePopupMenuImageList: TImageList;
    MainMenuImageList: TImageList;
    MainMenu: TMainMenu;
    miExportFileList: TMenuItem;
    miMain: TMenuItem;
    miTools: TMenuItem;
    miTreeExtract: TMenuItem;
    NTree2: TMenuItem;
    miExtractFile: TMenuItem;
    miTreeClear: TMenuItem;
    miPackImport: TMenuItem;
    NView2: TMenuItem;
    miTreeRename: TMenuItem;
    miClearFiles: TMenuItem;
    NView1: TMenuItem;
    miDeleteFile: TMenuItem;
    miAddFile: TMenuItem;
    miTreeAdd: TMenuItem;
    NTree1: TMenuItem;
    FolderViewOpenDialog: TOpenDialog;
    miTreeDelete: TMenuItem;
    N2: TMenuItem;
    miRenameFile: TMenuItem;
    miNew: TMenuItem;
    miOpen: TMenuItem;
    miSave: TMenuItem;
    miSaveAs: TMenuItem;
    miExit: TMenuItem;
    N1: TMenuItem;
    FolderTree: TTreeView;
    PackOpenDialog: TOpenDialog;
    FolderViewPopupMenu: TPopupMenu;
    FolderTreePopupMenu: TPopupMenu;
    PackSaveDialog: TSaveDialog;
    FolderViewExtractDialog: TSelectDirectoryDialog;
    PackListSaveDialog: TSaveDialog;
    Splitter: TSplitter;
    StatusBar: TStatusBar;
    procedure FolderTreeKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FolderTreeSelectionChanged(Sender: TObject);
    procedure FolderViewChange(Sender: TObject; Item: TListItem; Change: TItemChange);
    procedure FolderViewDblClick(Sender: TObject);
    procedure FolderViewKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FolderViewPopupMenuPopup(Sender: TObject);
    procedure FolderTreeClick(Sender: TObject);
    procedure FolderTreePopupMenuPopup(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
    procedure miExportFileListClick(Sender: TObject);
    procedure miExtractFileClick(Sender: TObject);
    procedure miPackImportClick(Sender: TObject);
    procedure miRenameFileClick(Sender: TObject);
    procedure miSaveAsClick(Sender: TObject);
    procedure miSaveClick(Sender: TObject);
    procedure miTreeAddClick(Sender: TObject);
    procedure miAddFileClick(Sender: TObject);
    procedure miClearFilesClick(Sender: TObject);
    procedure miDeleteFileClick(Sender: TObject);
    procedure miExitClick(Sender: TObject);
    procedure miNewClick(Sender: TObject);
    procedure miOpenClick(Sender: TObject);
    procedure miTreeClearClick(Sender: TObject);
    procedure miTreeDeleteClick(Sender: TObject);
    procedure miTreeExtractClick(Sender: TObject);
    procedure miTreeRenameClick(Sender: TObject);
  private
    FFileList: TsgePackFileWriter;  //Список файлов
    FModified: Boolean;             //Флаг изменения
    FProjectName: String;           //Имя проекта
    FProjectFileName: String;       //Имя архива на жёстком диске

    FTempDirectory: String;         //Временная папка

    procedure AddFileToList(TreePath: string; FileName: String);            //Добавить файл в проект
    function  GetPathFromTreeView(Node: TTreeNode): String;                 //Определить путь по Node
    function  GetPathFromTreeView: String;                                  //Определить путь из TreeView по выбранному узлу
    function  FolderExist(Node: TTreeNode; FolderName: String): Boolean;    //Узнать есть ли дубликат имени
    procedure AddPathToTree(Tree: TTreeView; Path: String);
    procedure RestoreTreePath(Path: String);
    procedure BuildView(Path: String = '');
    procedure BuildTree;
    procedure CorrectCaption;
    procedure CorrectStatusBar;

    procedure NewProject;
    procedure SaveProject;
    procedure SaveProjectAs;
    procedure OpenProject;
    procedure OpenProjectFromFile(FileName: String);


    procedure Tree_AddNode(TreeNodes: TTreeNodes; SelectedNode: TTreeNode; NodeName: String);
    procedure Tree_RenameNode;
    procedure Tree_DeleteNode;
    procedure Tree_ClearNodes;
    procedure Tree_Extract;

    procedure View_Extract;
    procedure View_AddPack;
    procedure View_AddFile;
    procedure View_Rename;
    procedure View_Delete;
    procedure View_Clear;
  public

  end;

var
  MainFrm: TMainFrm;



implementation

{$R *.lfm}

uses
  sgeErrors, sgeMemoryStream, sgeFile, sgePackFile, sgePackFileReader, sgeStringList,
  LazUTF8, LCLIntf;


const
  FormTitle = 'SGEPACK [v 0.5]';
  _UNITNAME = 'MainUnit';



//Поиск файлов в каталогах
procedure FindFilesInFolders(Path: String; List: TsgeStringList);
var
  o: TSearchRec;
  Idx: Integer;
begin
  Path := IncludeTrailingBackslash(Path);

  Idx := FindFirst(Path + '*', faAnyFile, o);
  while Idx = 0 do
    begin

    if (o.Name <> '.') and (o.Name <> '..') then
      begin
      if (o.Attr and faDirectory) = faDirectory then FindFilesInFolders(Path + o.Name, List)
        else List.Add(Path + o.Name);
      end;

    Idx := FindNext(o);
    end;

  FindClose(o);
end;


function GetBaseDirectoryPart(Directory: String): String;
var
  List: TsgeStringList;
begin
  Result := Directory;

  List := TsgeStringList.Create;
  List.Separator := '\';
  List.FromString(Directory);

  if List.Count >= 1 then
    begin
    List.Delete(List.Count - 1);
    Result := List.ToString;
    end;

  List.Free;
end;



procedure TMainFrm.miNewClick(Sender: TObject);
begin
  NewProject;
end;


procedure TMainFrm.miOpenClick(Sender: TObject);
begin
  OpenProject;
end;


procedure TMainFrm.miTreeClearClick(Sender: TObject);
begin
  Tree_ClearNodes;
end;


procedure TMainFrm.miTreeDeleteClick(Sender: TObject);
begin
  Tree_DeleteNode;
end;


procedure TMainFrm.miTreeExtractClick(Sender: TObject);
begin
  Tree_Extract;
end;


procedure TMainFrm.miTreeRenameClick(Sender: TObject);
begin
  Tree_RenameNode;
end;


procedure TMainFrm.AddFileToList(TreePath: string; FileName: String);
var
  Fl: TsgePackFileWriterBlock;
  f: TsgeFile;
begin
  if TreePath <> '' then TreePath := IncludeTrailingBackslash(TreePath);

  //Подготовить запись
  Fl.PackName := TreePath + ExtractFileName(FileName);
  Fl.FileType := pftFile;
  Fl.FileName := FileName;
  Fl.Index := 0;

  //Получить размер файла
  try
    f := TsgeFile.Create(FileName, fmRead, False);
    Fl.FileSize := f.Size;
    f.Free;
  except
    Exit;
  end;

  //Добавить в проект
  FFileList.Add(Fl);
end;


function TMainFrm.GetPathFromTreeView(Node: TTreeNode): String;
begin
  Result := '';

  //Построить путь
  while Node.Parent <> nil do
    begin
    Result := Node.Text + '\' + Result;
    Node := Node.Parent;
    end;
end;


function TMainFrm.GetPathFromTreeView: String;
var
  Node: TTreeNode;
begin
  Result := '';
  Node := FolderTree.Selected;
  if Node = nil then Exit;

  Result := GetPathFromTreeView(Node);
end;


function TMainFrm.FolderExist(Node: TTreeNode; FolderName: String): Boolean;
var
  i, c: Integer;
begin
  Result := False;

  FolderName := UTF8LowerCase(FolderName);
  c := Node.Count - 1;
  for i := 0 to c do
    if UTF8LowerCase(Node.Items[i].Text) = FolderName then
      begin
      Result := True;
      Break;
      end;
end;


procedure TMainFrm.AddPathToTree(Tree: TTreeView; Path: String);
var
  //sa: TStringArray;
  List: TsgeStringList;
  Count, i: Integer;
  RootNode, Node: TTreeNode;
begin
  List := TsgeStringList.Create;
  List.Separator := '\';
  List.FromString(Path);
  Count := List.Count - 1;

  //Цикл по вложенности
  RootNode := Tree.Items.GetFirstNode;
  if Count > 0 then
    for i := 0 to Count - 1 do
      begin
      //Поиск узла по имени
      Node := RootNode.FindNode(List.Part[i] {sa[i]});

      if Node = nil then
        begin
        //Не найден узел
        RootNode := Tree.Items.AddChild(RootNode, List.Part[i] {sa[i]});
        RootNode.ImageIndex := 0;
        RootNode.SelectedIndex := 1;
        end
        else begin
        //Найден
        RootNode := Node;
        end;
      end;

  List.Free;
end;


procedure TMainFrm.RestoreTreePath(Path: String);
var
  i, c, Idx: Integer;
  Node: TTreeNode;
  List: TsgeStringList;
begin
  List := TsgeStringList.Create;
  List.Separator := '\';
  List.FromString(Path);

  Node := FolderTree.Items.GetFirstNode;

  c := List.Count - 1;

  for i := 0 to c do
    begin
    Idx := Node.IndexOfText(List.Part[i]);
    if idx <> -1 then Node := Node.Items[Idx];
    end;

  Node.Selected := True;

  List.Free;
end;


procedure TMainFrm.BuildView(Path: String);
var
  i, c: Integer;
  s: String;
  Li: TListItem;
begin
  FolderView.Items.BeginUpdate;
  FolderView.Clear;

  c := FFileList.Count - 1;
  Path := UTF8LowerCase(Path);
  for i := 0 to c do
    begin
    s := UTF8LowerCase(ExtractFilePath(FFileList.Item[i].PackName));

    if Path = s then
      begin
      Li := FolderView.Items.Add;
      Li.Caption := ExtractFileName(FFileList.Item[i].PackName);
      Li.ImageIndex := 0;
      Li.StateIndex := 0;
      Li.SubItems.Add(IntToStr(FFileList.Item[i].FileSize));
      end;
    end;

  FolderView.Items.EndUpdate;

  CorrectStatusBar;
end;


procedure TMainFrm.BuildTree;
var
  Node: TTreeNode;
  i, c: Integer;
  Path: String;
begin
  //Запомнить текущий путь
  Path := GetPathFromTreeView;


  FolderTree.Items.BeginUpdate;
  FolderTree.Items.Clear;

  //Добавить основной узел
  Node := FolderTree.Items.Add(FolderTree.Items.GetFirstNode, '');
  Node.Text := '\';
  Node.ImageIndex := 0;
  Node.SelectedIndex := 1;
  Node.Selected := True;

  //Цикл по именам архива
  c := FFileList.Count - 1;
  for i := 0 to c do
    AddPathToTree(FolderTree, FFileList.Item[i].PackName);

  FolderTree.Items.EndUpdate;

  //Восстановить путь
  RestoreTreePath(Path);
end;



procedure TMainFrm.miDeleteFileClick(Sender: TObject);
begin
  View_Delete;
end;


procedure TMainFrm.miExitClick(Sender: TObject);
begin
  Close;
end;


procedure TMainFrm.FormCreate(Sender: TObject);
var
  fn: String;
begin
  FFileList := TsgePackFileWriter.Create;

  FTempDirectory := GetTempDir(True);
  if FTempDirectory = '' then
    begin
    ShowMessage('Невозможно получить каталог по умолчанию');
    halt;
    end;


  //Создать новый проект
  NewProject;


  //Проверить на открытие архива
  fn := ParamStr(1);
  if FileExists(fn) then OpenProjectFromFile(fn) else;
end;


procedure TMainFrm.FormDestroy(Sender: TObject);
begin
  FFileList.Free;
end;


procedure TMainFrm.FormDropFiles(Sender: TObject; const FileNames: array of String);
var
  Path, BasePath, TreePath: String;
  List: TsgeStringList;
  i, j, c, k: Integer;
begin
  Path := GetPathFromTreeView;
  List := TsgeStringList.Create;

  try
    c := Length(FileNames) - 1;
    for i := 0 to c do
      begin
      List.Clear;

      if DirectoryExists(FileNames[i]) then
        begin
        BasePath := GetBaseDirectoryPart(FileNames[i]);     //Найти базовый каталог
        FindFilesInFolders(FileNames[i], List {@sa});       //Поиск всех файлов в каталоге

        k := List.Count - 1;
        for j := 0 to k do
          begin
          TreePath := List.Part[j];
          Delete(TreePath, 1, Length(BasePath) + 1);
          TreePath := ExcludeTrailingBackslash(Path + ExtractFilePath(TreePath));
          AddFileToList(TreePath, List.Part[j]);
          end;

        end else AddFileToList(Path, FileNames[i]);

      end;



  except
    on E: EsgeException do
      begin
      ShowMessage(E.Message);
      Exit;
      end;
  end;


  List.Free;

  //Всякая шляпа
  FModified := True;
  BuildTree;
  BuildView(Path);
  CorrectStatusBar;
end;


procedure TMainFrm.miExportFileListClick(Sender: TObject);
var
  List: TsgeStringList;
  i, c: Integer;
begin
  PackListSaveDialog.FileName := FProjectName;
  if PackListSaveDialog.Execute then
    begin
    List := TsgeStringList.Create;

    //Подготовить список
    c := FFileList.Count - 1;
    for i := 0 to c do
      List.Add(FFileList.Item[i].PackName);


    //Сохранить
    try
      try
        List.SaveToFile(PackListSaveDialog.FileName);
      except
        ShowMessage('Не могу сохранить список в файл "' + PackListSaveDialog.FileName + '"');
      end;

    finally
      List.Free;
    end;


    end;
end;


procedure TMainFrm.miExtractFileClick(Sender: TObject);
begin
  View_Extract;
end;


procedure TMainFrm.miPackImportClick(Sender: TObject);
begin
  View_AddPack;
end;


procedure TMainFrm.miRenameFileClick(Sender: TObject);
begin
  View_Rename;
end;


procedure TMainFrm.miSaveAsClick(Sender: TObject);
begin
  SaveProjectAs;
end;


procedure TMainFrm.miSaveClick(Sender: TObject);
begin
  SaveProject;
end;


procedure TMainFrm.miTreeAddClick(Sender: TObject);
var
  SelectNode: TTreeNode;
begin
  SelectNode := FolderTree.Selected;
  if SelectNode = nil then Exit;


  Tree_AddNode(FolderTree.Items, SelectNode, 'Новый');
end;


procedure TMainFrm.miAddFileClick(Sender: TObject);
begin
  View_AddFile;
end;


procedure TMainFrm.miClearFilesClick(Sender: TObject);
begin
  View_Clear;
end;


procedure TMainFrm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if FModified and (MessageDlg('Вопрос', 'Сохранить архив?', mtConfirmation, mbYesNo, 0) = mrYes) then SaveProject;
end;


procedure TMainFrm.FolderTreeClick(Sender: TObject);
var
  Path: String;
  Node: TTreeNode;
begin
  Node := TTreeView(Sender).Selected;

  if Node = nil then Exit;

  Path := GetPathFromTreeView;

  BuildView(Path);
end;


procedure TMainFrm.FolderTreePopupMenuPopup(Sender: TObject);
var
  b: Boolean;
begin
  b := (FolderTree.Selected <> nil);

  miTreeAdd.Enabled := b;
  miTreeRename.Enabled := b and (FolderTree.Selected.Text <> '\');
  miTreeDelete.Enabled := miTreeRename.Enabled;
end;


procedure TMainFrm.FolderViewPopupMenuPopup(Sender: TObject);
begin
  //Удалить
  miDeleteFile.Enabled := (FolderView.SelCount > 0);

  //Очистить
  miClearFiles.Enabled := (FolderView.Items.Count > 0);

  //Переименовать
  miRenameFile.Enabled := (FolderView.SelCount = 1);

  //Извлечь
  miExtractFile.Enabled := (FolderView.SelCount > 0);
end;


procedure TMainFrm.FolderViewKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_A      : if (ssCtrl in Shift) then FolderView.SelectAll;
    VK_DELETE : View_Delete;
    VK_F2     : View_Rename;
  end;
end;


procedure TMainFrm.FolderTreeKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_DELETE     : Tree_DeleteNode;
    VK_F2         : Tree_RenameNode;
  end;
end;


procedure TMainFrm.FolderTreeSelectionChanged(Sender: TObject);
begin
  BuildView(GetPathFromTreeView);
end;


procedure TMainFrm.FolderViewChange(Sender: TObject; Item: TListItem; Change: TItemChange);
begin
  CorrectStatusBar;
end;


procedure TMainFrm.FolderViewDblClick(Sender: TObject);
var
  Fn, PackFn: String;
  Idx: Integer;
  Ms: TsgeMemoryStream;
  F: TsgePackFileReader;
begin
  if FolderView.SelCount <> 1 then Exit;

  //Полное имя файла
  PackFn := GetPathFromTreeView + FolderView.ItemFocused.Caption;

  //Индекс в массиве
  Idx := FFileList.IndexOf(PackFn);

  if Idx = -1 then Exit;

  try
    try
      //Распаковать файл
      Ms := TsgeMemoryStream.Create;
      case FFileList.Item[Idx].FileType of
        pftFile:
          Ms.LoadFromFile(FFileList.Item[Idx].FileName);

        pftPack:
          begin
          F := TsgePackFileReader.Create(FFileList.Item[Idx].FileName);
          F.GetItemData(FFileList.Item[Idx].Index, Ms);
          FreeAndNil(F);
          end;
      end;
      Fn := FTempDirectory + FolderView.ItemFocused.Caption;
      Ms.SaveToFile(Fn);
    except
      ShowMessage('Не могу распаковать файл "' + PackFn + '"');
    end;

  finally
    Ms.Free;
  end;


  //Запустить
  OpenDocument(Fn);
end;


procedure TMainFrm.CorrectCaption;
begin
  //Caption
  if FProjectName <> '' then Caption := FProjectName + ' - ' + FormTitle else Caption := FormTitle;
end;


procedure TMainFrm.CorrectStatusBar;
begin
  //Total
  StatusBar.Panels[1].Text := IntToStr(FFileList.Count);

  //View
  StatusBar.Panels[3].Text := IntToStr(FolderView.Items.Count);
  StatusBar.Panels[5].Text := IntToStr(FolderView.SelCount);
end;


procedure TMainFrm.NewProject;
begin
  if FModified and (MessageDlg('Вопрос', 'Сохранить архив?', mtConfirmation, mbYesNo, 0) = mrYes) then SaveProject;


  //Устанавливаем параметры проекта
  FModified := False;
  FProjectFileName := '';
  FProjectName := 'Новый';
  FFileList.Clear;


  //Обновить интерфейс
  BuildTree;
  BuildView;
  CorrectCaption;
  CorrectStatusBar;
end;


procedure TMainFrm.SaveProject;
var
  F: TsgeFile;
  FPack: TsgePackFileReader;
  Ms: TsgeMemoryStream;
  i, c: Integer;
  FileHead: TsgePackFileHeader;
  BlockHead: TsgePackFileBlock;
  Cancel: Boolean;
  Fn, TempFn: String;
begin
  if FModified then
    begin
    FPack := nil;
    Ms := nil;
    Cancel := False;

    //Определить имя файла
    if FProjectFileName = '' then
      begin
      PackSaveDialog.FileName := FProjectName;
      if not PackSaveDialog.Execute then Exit;
      Fn := PackSaveDialog.FileName;
      end else Fn := FProjectFileName;
    TempFn := ChangeFileExt(Fn, '.tmp');


    //Показать прогресс
    ProgressFrm.SetUp(FFileList.Count);


    try

      try
        F := TsgeFile.Create(TempFn, fmWrite, True);
        F.Size := 0;

        //Записать заголовок
        FileHead := sgePackFile_GetFileHead;
        F.Write(FileHead, SizeOf(TsgePackFileHeader));

        //Записать файлы в архив
        c := FFileList.Count - 1;
        for i := 0 to c do
          begin
          Ms := TsgeMemoryStream.Create;

          try
            //Прочитать данные в память
            case FFileList.Item[i].FileType of
              pftFile:
                Ms.LoadFromFile(FFileList.Item[i].FileName);

              pftPack:
                begin
                FPack := TsgePackFileReader.Create(FFileList.Item[i].FileName);
                FPack.GetItemData(FFileList.Item[i].Index, Ms);
                FreeAndNil(FPack);
                end;
            end;

          except
            if MessageDlg('Вопрос...', 'Не могу открыть файл для чтения. Прервать?' + #13#10 + FFileList.Item[i].FileName, mtConfirmation, mbYesNo, 0) = mrYes then
              begin
              Cancel := True;
              Break;
              end;
          end;


          //Записать блок 1 уровня
          BlockHead.NameSize := Length(FFileList.Item[i].PackName);
          BlockHead.TotalSize := BlockHead.NameSize + SizeOf(TsgePackFileBlock) + FFileList.Item[i].FileSize;
          F.Write(BlockHead, SizeOf(TsgePackFileBlock));

          //Записать файл
          F.Write(Ms.Data^, Ms.Size);

          //Записать имя файла
          F.Write(FFileList.Item[i].PackName[1], BlockHead.NameSize);

          //Прибить стрим
          FreeAndNil(Ms);


          //Интерфейс
          ProgressFrm.Step;
          Application.ProcessMessages;
          if not ProgressFrm.Visible then
            begin
            Cancel := True;
            Break;
            end;

          end;  //For


      except
        on E: EsgeException do
          begin
          ShowMessage(E.Message);
          end;
      end;


    finally
      F.Free;
      FPack.Free;
      Ms.Free;
      ProgressFrm.Hide;
    end;


    if Cancel then
      begin
      ShowMessage('Операция сохранения прервана!');
      DeleteFile(TempFn);
      end
      else begin
      //Поменять файлы местами
      if FileExists(Fn) then
        begin
        //Удалить старый файл проекта
        if not DeleteFile(Fn) then
          begin
          ShowMessage('Не могу удалить старый архив!' + #13#10 + Fn);
          DeleteFile(TempFn);
          Exit;
          end;
        end;

      //Заменить временный файл проекта новым
      if not RenameFile(TempFn, Fn) then
        begin
        ShowMessage('Не могу переименовать архив!' + #13#10 + TempFn);
        Exit;
        end;

      //Поправить параметры проекта
      FModified := False;
      FProjectFileName := Fn;
      FProjectName := ChangeFileExt(ExtractFileName(FProjectFileName), '');
      CorrectCaption;
      end;


    end;
end;


procedure TMainFrm.SaveProjectAs;
begin
  FProjectFileName := '';
  FModified := True;
  SaveProject;
end;


procedure TMainFrm.OpenProject;
begin
  if FModified and (MessageDlg('Вопрос', 'Сохранить архив?', mtConfirmation, mbYesNo, 0) = mrYes) then SaveProject;

  if PackOpenDialog.Execute then
    OpenProjectFromFile(PackOpenDialog.FileName);
end;


procedure TMainFrm.OpenProjectFromFile(FileName: String);
var
  F: TsgePackFileReader;
  Item: TsgePackFileWriterBlock;
  i, c: Integer;
begin
  F := nil;

  try
    try
      F := TsgePackFileReader.Create(FileName);

      //Цикл по элементам
      FFileList.Clear;
      c := F.Count - 1;
      for i := 0 to c do
        begin
        Item.FileName := FileName;
        Item.FileSize := F.Item[i].DataSize;
        Item.FileType := pftPack;
        Item.Index := i;
        Item.PackName := F.Item[i].FileName;

        //Добавить в список
        FFileList.Add(Item);
        end;

    except
      on E: EsgeException do
        begin
        ShowMessage(E.Message);
        Exit;
        end;
    end;


  finally
    F.Free;
  end;


  //Поправить интерфейс
  BuildTree;
  BuildView;
  FProjectName := ChangeFileExt(ExtractFileName(FileName), '');
  FProjectFileName := FileName;
  CorrectCaption;
  CorrectStatusBar;
  FModified := False;
end;


procedure TMainFrm.Tree_AddNode(TreeNodes: TTreeNodes; SelectedNode: TTreeNode; NodeName: String);
var
  Node: TTreeNode;
  Idx: Integer;
  s: String;
begin
  //Подготовить уникальное имя узла
  Idx := 0;
  s := NodeName + #32 + IntToStr(Idx);
  while SelectedNode.IndexOfText(s) <> -1 do
    begin
    Inc(Idx);
    s := NodeName + #32 + IntToStr(Idx);
    end;


  //Добавить узел
  Node := TreeNodes.AddChild(SelectedNode, s);
  Node.ImageIndex := 0;
  Node.SelectedIndex := 1;

  //Всякая шляпа
  FModified := True;
end;


procedure TMainFrm.Tree_RenameNode;
label
  A1;
var
  Node: TTreeNode;
  BasePath, OldName, NewName: String;
  i, c, OldPathSize: Integer;
  s, s2: String;
  FL: TsgePackFileWriterBlock;
begin
  Node := FolderTree.Selected;
  if Node = nil then Exit;
  if Node.Text = '\' then Exit;

  //Определить путь без имени катлога
  BasePath := GetPathFromTreeView(Node.Parent);   //Путь без имени текущего каталога
  OldName := UTF8LowerCase(BasePath + Node.Text); //Полный путь
  OldPathSize := UTF8Length(OldName);             //Размер пути
  NewName := Node.Text;

A1:
  if InputQuery('Укажите новое имя каталога', 'Имя каталога', NewName) then
    begin
    //Проверить имя на уникальность
    if FolderExist(Node.Parent, NewName) then
      begin
      ShowMessage('Каталог "' + NewName + '" существует, укажите другое имя.');
      goto A1;
      end;


    //Поправить Tree
    Node.Text := NewName;
    NewName := BasePath + NewName;

    //Просмотреть файлы на совпадение
    c := FFileList.Count - 1;
    for i := 0 to c do
      begin
      s := UTF8LowerCase(FFileList.Item[i].PackName);
      if UTF8Pos(OldName, s) = 1 then
        begin
        s2 := FFileList.Item[i].PackName;
        UTF8Delete(s2, 1, OldPathSize);
        s2 := NewName + s2;

        FL := FFileList.Item[i];
        FL.PackName := s2;
        FFileList.Item[i] := FL;
        end;
      end;



    //Всякая шляпа
    FModified := True;
    end;
end;


procedure TMainFrm.Tree_DeleteNode;
var
  Path, s: String;
  i: Integer;
  Node: TTreeNode;
begin
  Node := FolderTree.Selected;
  if Node = nil then Exit;
  if Node.Text = '\' then Exit;


  if MessageDlg('Вопрос', 'Удалить каталог? "' + Node.Text + '"' + #13#10 + 'Внимание буду удалены дочерние элементы!', mtConfirmation, mbYesNo, 0) = mrNo then Exit;

  Path := UTF8LowerCase(GetPathFromTreeView);

  //Удалить файлы с вхождением Path
  i := -1;
  while i < FFileList.Count - 1 do
    begin
    Inc(i);

    s := UTF8LowerCase(FFileList.Item[i].PackName);
    if UTF8Pos(Path, s) = 1 then
      begin
      FFileList.Delete(i);
      Dec(i)
      end;
    end;

  //Всякая шляпа
  BuildTree;
  BuildView(GetPathFromTreeView);
  FModified := True;
  CorrectStatusBar;
end;


procedure TMainFrm.Tree_ClearNodes;
var
  Node: TTreeNode;
  i: Integer;
begin
  if MessageDlg('Вопрос', 'Удалить все каталоги кроме корня?', mtConfirmation, mbYesNo, 0) = mrNo then Exit;

  //Удалить все файлы с каталогами
  i := -1;
  while i < FFileList.Count - 1 do
    begin
    Inc(i);

    if Pos('\', FFileList.Item[i].PackName) <> 0 then
      begin
      FFileList.Delete(i);
      Dec(i)
      end;
    end;

  //Удалить Node
  Node := FolderTree.Items.GetFirstNode;
  Node.DeleteChildren;
  Node.Selected := True;

  //Всякая шляпа
  FModified := True;
  BuildView;
  CorrectStatusBar;
end;


procedure TMainFrm.Tree_Extract;
var
  i, c: Integer;
  Path: String;
  Ms: TsgeMemoryStream;
  SavePath, fn, s: String;
  F: TsgePackFileReader;
  B: Boolean;
begin
  F := Nil;
  Ms := Nil;

  //Узнать текущий путь в дереве
  Path := GetPathFromTreeView;


  //Узнать количество извлекаемых файлов
  if Path = '' then c := FFileList.Count
    else begin
    c := 0;
    s := UTF8LowerCase(Path);
    for i := 0 to FFileList.Count - 1 do
      if UTF8Pos(s, UTF8LowerCase(FFileList.Item[i].PackName)) = 1 then Inc(c);
    end;


  //Нет файлов для извлечения
  if c = 0 then
    begin
    ShowMessage('Нет файлов для извлечения');
    Exit;
    end;


  //Выбрать каталог
  if FolderViewExtractDialog.Execute then
    begin
    //Подготовить путь для сохранения
    SavePath := IncludeTrailingBackslash(FolderViewExtractDialog.FileName);

    //Показать форму прогресса
    ProgressFrm.SetUp(c);


    try

      try
        //Найти выделенные файлы и извлечь
        c := FFileList.Count - 1;
        for i := 0 to c do
          begin
          B := False;
          if Path = '' then B := True else
            if UTF8Pos(s, UTF8LowerCase(FFileList.Item[i].PackName)) = 1 then B := True;

          if B then
            begin
            //Читаем данные в память
            Ms := TsgeMemoryStream.Create;
            case FFileList.Item[i].FileType of
              pftFile:
                Ms.LoadFromFile(FFileList.Item[i].FileName);

              pftPack:
                begin
                F := TsgePackFileReader.Create(FFileList.Item[i].FileName);
                F.GetItemData(FFileList.Item[i].Index, Ms);
                FreeAndNil(F);
                end;
            end;

            //Проверить на существование файла
            fn := FFileList.Item[i].PackName;
            UTF8Delete(fn, 1, Length(Path));

            fn := SavePath + fn{FFileList.Item[i].PackName};          //Определить имя файла
            ForceDirectories(ExtractFilePath(fn));                //Создать дерево каталогов
            if FileExists(fn) then
              if MessageDlg('Вопрос', 'Файл "' + ExtractFileName(fn) + '" существует. Переписать?', mtConfirmation, mbYesNo, 0) = mrNo then Continue;


            //Сохранить в папку
            Ms.SaveToFile(fn);
            FreeAndNil(Ms);

            //Проверить форму
            ProgressFrm.Step;
            Application.ProcessMessages;
            if not ProgressFrm.Visible then
              begin
              ShowMessage('Извлечение прервано!');
              Break;
              end;
            end;


          end;//For

      except
        on E: EsgeException do
          EsgeException.Create(_UNITNAME, 'Err_ExtractError', fn, E.Message);
      end;


    finally
      Ms.Free;
      F.Free;
    end;


    //Закрыть прогресс
    ProgressFrm.Hide;
    end;
end;


procedure TMainFrm.View_Extract;
var
  i, c, Idx: Integer;
  Path, PackName: String;
  Ms: TsgeMemoryStream;
  SavePath, fn: String;
  F: TsgePackFileReader;
begin
  if FolderView.SelCount < 1 then Exit;


  if FolderViewExtractDialog.Execute then
    begin
    //Подготовить путь для сохранения
    SavePath := IncludeTrailingBackslash(FolderViewExtractDialog.FileName);

    //Узнать текущий путь в дереве
    Path := GetPathFromTreeView;

    //Показать форму прогресса
    ProgressFrm.SetUp(FolderView.SelCount);


    try

      try
        //Найти выделенные файлы и извлечь
        c := FolderView.Items.Count - 1;
        for i := 0 to c do
          if FolderView.Items.Item[i].Selected then
            begin
            PackName := Path + FolderView.Items.Item[i].Caption;  //Узнать имя в проекте
            Idx := FFileList.IndexOf(PackName);                   //Индекс файла
            if Idx = -1 then Continue;


            //Читаем данные в память
            Ms := TsgeMemoryStream.Create;
            case FFileList.Item[Idx].FileType of
              pftFile:
                Ms.LoadFromFile(FFileList.Item[Idx].FileName);


              pftPack:
                begin
                F := TsgePackFileReader.Create(FFileList.Item[Idx].FileName);
                F.GetItemData(FFileList.Item[Idx].Index, Ms);
                FreeAndNil(F);
                end;
            end;

            //Проверить на существование файла
            fn := SavePath + FolderView.Items.Item[i].Caption;
            if FileExists(fn) then
              if MessageDlg('Вопрос', 'Файл "' + FolderView.Items.Item[i].Caption + '" существует. Переписать?', mtConfirmation, mbYesNo, 0) = mrNo then Continue;


            //Сохранить в папку
            Ms.SaveToFile(fn);
            FreeAndNil(Ms);

            //Проверить форму
            ProgressFrm.Step;
            Application.ProcessMessages;
            if not ProgressFrm.Visible then
              begin
              ShowMessage('Извлечение прервано!');
              Break;
              end;
            end;


      except
        on E: EsgeException do
          begin
          ShowMessage(E.Message);
          end;
      end;


    finally
      Ms.Free;
      F.Free;
    end;


    //Закрыть прогресс
    ProgressFrm.Hide;
    end;
end;


procedure TMainFrm.View_AddPack;
var
  i, c, j, k: Integer;
  F: TsgePackFileReader;
  Path, Fn: String;
  Item: TsgePackFileWriterBlock;
begin
  if FolderViewOpenDialog.Execute then
    begin
    F := Nil;

    //Путь в дереве
    Path := GetPathFromTreeView;

    //Цикл по открываемым файлам
    c := FolderViewOpenDialog.Files.Count - 1;
    for i := 0 to c do
      begin
      Fn := FolderViewOpenDialog.Files.Strings[i];

      try
        try
          F := TsgePackFileReader.Create(Fn);

          //Цикл по файлам внутри архива
          k := F.Count - 1;
          for j := 0 to k do
            begin
            //Подготовить запись
            Item.FileName := Fn;
            Item.FileSize := F.Item[j].DataSize;
            Item.FileType := pftPack;
            Item.Index := j;
            Item.PackName := Path + F.Item[j].FileName;

            //Добавить в проект
            FFileList.Add(Item);
            end;

          FreeAndNil(F);

        except
          on E: EsgeException do
            begin
            ShowMessage(E.Message);
            Exit;
            end;
        end;


      finally
        F.Free;
      end;


      end;

    //Всякая шляпа
    FModified := True;
    BuildTree;
    BuildView(Path);
    CorrectStatusBar;
    end;
end;


procedure TMainFrm.View_AddFile;
var
  Files: array of String;
  i: Integer;
begin
  if FolderViewOpenDialog.Execute then
    begin
    SetLength(Files, FolderViewOpenDialog.Files.Count);

    for i := 0 to FolderViewOpenDialog.Files.Count - 1 do
      Files[i] := FolderViewOpenDialog.Files.Strings[i];

    FormDropFiles(Self, Files); //Грязный хак
    SetLength(Files, 0);
    end;
end;


procedure TMainFrm.View_Rename;
var
  Li: TListItem;
  Idx: Integer;
  Nm, Path, Ext: String;
  F: TsgePackFileWriterBlock;
begin
  if FolderView.SelCount = 0 then Exit;


  Li := FolderView.Selected;

  if Li = nil then Exit;

  Path := GetPathFromTreeView;

  //Найти индекс в проекте
  Idx := FFileList.IndexOf(Path + Li.Caption);

  //Спросить новое имя
  Ext := ExtractFileExt(Li.Caption);
  Nm := ChangeFileExt(Li.Caption, '');
  if InputQuery('Укажите новое имя файла', 'Имя файла', Nm) then
    begin
    Nm := ChangeFileExt(Nm, Ext);
    F := FFileList.Item[Idx];
    F.PackName := Path + Nm;
    FFileList.Item[Idx] := F;
    Li.Caption := Nm;

    //Всякая шляпа
    FModified := True;
    end;
end;


procedure TMainFrm.View_Delete;
var
  i, c: Integer;
  Path: String;
begin
  if FolderView.SelCount = 0 then Exit;


  if MessageDlg('Вопрос', 'Удалить выбранные файлы?', mtConfirmation, mbYesNo, 0) = mrNo then Exit;

  Path := GetPathFromTreeView;

  c := FolderView.Items.Count - 1;
  for i := 0 to c do
    if FolderView.Items.Item[i].Selected then
      FFileList.Delete(Path + FolderView.Items.Item[i].Caption);

  //Всякая шляпа
  FModified := True;
  BuildView(Path);
  CorrectStatusBar;
end;


procedure TMainFrm.View_Clear;
var
  i, c: Integer;
  Path: String;
begin
  if MessageDlg('Вопрос', 'Удалить файлы в текущем каталоге?', mtConfirmation, mbYesNo, 0) = mrNo then Exit;

  Path := GetPathFromTreeView;

  c := FolderView.Items.Count - 1;
  for i := 0 to c do
    FFileList.Delete(Path + FolderView.Items.Item[i].Caption);


  //Всякая шляпа
  FModified := True;
  BuildView(Path);
  CorrectStatusBar;
end;


end.

