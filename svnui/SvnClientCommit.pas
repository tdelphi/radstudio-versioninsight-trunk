unit SvnClientCommit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, CheckLst, SvnClient, ComCtrls, ToolsApi;

type
  TSvnCommitForm = class(TForm)
    Label1: TLabel;
    Location: TLabel;
    Comment: TMemo;
    Recent: TButton;
    Help: TButton;
    Cancel: TButton;
    OK: TButton;
    Label2: TLabel;
    Files: TListView;
    procedure OKClick(Sender: TObject);
    procedure CommentKeyPress(Sender: TObject; var Key: Char);
    procedure FilesCompare(Sender: TObject; Item1, Item2: TListItem;
      Data: Integer; var Compare: Integer);
    procedure FilesColumnClick(Sender: TObject; Column: TListColumn);
  protected
    FSvnClient: TSvnClient;
    FProject: IOTAProject;
    procedure ModificationCallBack(Sender: TObject; Item: TSvnItem; var Cancel: Boolean);
  public
    constructor Create(AOwner: TComponent; ASvnClient: TSvnClient;
      const MenuContextList: IInterfaceList); reintroduce;
  end;

implementation

{$R *.dfm}

uses svn_client;

{ TSvnCommitForm }

procedure TSvnCommitForm.CommentKeyPress(Sender: TObject; var Key: Char);
begin
  inherited;
  Ok.Enabled := Comment.Text <> '';
end;

function CompareColumn(Item1, Item2: TListItem; Data: Integer): Integer; stdcall;
var
  S1, S2: string;
begin
  if Data = 0 then
  begin
    S1 := Item1.Caption;
    S2 := Item2.Caption;
  end
  else
  begin
    S1 := Item1.SubItems[Data];
    S2 := Item2.SubItems[Data];
  end;
  Result := CompareText(S1, S2);
end;

constructor TSvnCommitForm.Create(AOwner: TComponent;
  ASvnClient: TSvnClient; const MenuContextList: IInterfaceList);
var
  I, J: Integer;
  MenuContext: IOTAMenuContext;
  S: string;
  DirectoryList: TStringList;
begin
  inherited Create(AOwner);
  FSvnClient := ASvnClient;
  DirectoryList := TStringList.Create;
  try
    for I := 0 to MenuContextList.Count - 1 do
    begin
      if FProject = nil then
        FProject := (MenuContextList[I] as IOTAProjectMenuContext).Project;
      if Supports(MenuContextList[I], IOTAMenuContext, MenuContext) then
        if FileExists(MenuContext.Ident) then
        begin
          // If it is a project
          if Supports((BorlandIDEServices as IOTAModuleServices).FindModule(MenuContext.Ident), IOTAProject) then
          begin
            DirectoryList.Add(ExtractFilePath(MenuContext.Ident));
            for J := 0 to FProject.GetModuleCount - 1 do
            begin
              S := ExtractFilePath(FProject.GetModule(J).FileName);
              if (S <> '') and (DirectoryList.IndexOf(S) = -1) then
                DirectoryList.Add(S);
            end;
          end
          else
            FProject.GetAssociatedFiles(MenuContext.Ident, DirectoryList);
        end;
    end;
    for I := 0 to DirectoryList.Count - 1 do
      ASvnClient.GetModifications(DirectoryList[I], ModificationCallBack, False);
  finally
    DirectoryList.Free;
  end;
end;

procedure TSvnCommitForm.FilesColumnClick(Sender: TObject; Column: TListColumn);
begin
  Files.CustomSort(@CompareColumn, Column.Tag);
end;

procedure TSvnCommitForm.FilesCompare(Sender: TObject; Item1, Item2: TListItem;
  Data: Integer; var Compare: Integer);
var
  S1, S2: string;
begin
  S1 := Item1.Caption;
  S2 := Item2.Caption;
  Compare := CompareText(S1, S2);
end;

procedure TSvnCommitForm.ModificationCallBack(Sender: TObject; Item: TSvnItem;
  var Cancel: Boolean);
var
  ListItem: TListItem;
begin
  ListItem := Files.Items.Add;
  ListItem.Caption := ExtractFileName(Item.PathName);
  ListItem.SubItems.Add(ExtractFilePath(Item.PathName));
  ListItem.SubItems.Add(ExtractFileExt(Item.PathName));
  ListItem.SubItems.Add(StatusKindStr(Item.TextStatus));
  ListItem.Checked := Item.TextStatus <> svnWcStatusUnversioned;
end;

procedure TSvnCommitForm.OKClick(Sender: TObject);
var
  CommitList: TStringList;
  I: Integer;
begin
  CommitList := TStringList.Create;
  try
    for I := 0 to Files.Items.Count - 1 do
      if Files.Items[I].Checked then
        CommitList.Add(Files.Items[I].SubItems[0] + Files.Items[I].Caption);
    FSvnClient.Commit(CommitList, Comment.Text);
  finally
    CommitList.Free;
  end;
end;

end.
