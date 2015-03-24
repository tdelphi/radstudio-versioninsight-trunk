unit GitIDESavedRepoInfo;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ComCtrls;

type
  TfrmSavedRepoInfo = class(TForm)
    ListView1: TListView;
    Panel1: TPanel;
    BtnOk: TButton;
    BtnCancel: TButton;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

  function ShowGitSavedRepoInfo: TModalResult;

var
  frmSavedRepoInfo: TfrmSavedRepoInfo;

implementation

uses System.Win.Registry, ToolsApi, GitIDEConst;

{$R *.dfm}

function ShowGitSavedRepoInfo: TModalResult;
var
  FrmSavedRepoInfo: TfrmSavedRepoInfo;
  RegIniFile: TRegIniFile;
  I: Integer;
  Key: string;
begin
  FrmSavedRepoInfo := TfrmSavedRepoInfo.Create(nil);

  try
    Result := FrmSavedRepoInfo.ShowModal;
    if Result = mrOK then
    begin
      for I := 0 to FrmSavedRepoInfo.ListView1.Items.Count -1 do
      begin
        Key := (BorlandIDEServices as IOTAServices).GetBaseRegistryKey + sGitRegBaseKey;
        RegIniFile := TRegIniFile.Create(Key);
        try
          if FrmSavedRepoInfo.ListView1.Items[I].Checked then
            RegIniFile.EraseSection(FrmSavedRepoInfo.ListView1.Items[I].Caption);
        finally
          RegIniFile.Free;
        end;
      end;
    end;
  finally
    FrmSavedRepoInfo.Free;
  end;

end;

procedure TfrmSavedRepoInfo.FormCreate(Sender: TObject);
var
  RegIniFile: TRegIniFile;
  SubKeyNames: TStringList;
  ListItem: TListItem;
  Key: string;
  SubKey: string;
  RemoteUrl: string;
begin
  Key := (BorlandIDEServices as IOTAServices).GetBaseRegistryKey + sGitRegBaseKey;
  RegIniFile := TRegIniFile.Create(Key);
  SubKeyNames := TStringList.Create;
  try
    RegIniFile.GetKeyNames(SubKeyNames);
    for SubKey in SubKeyNames do
    begin
      RemoteUrl := RegIniFile.ReadString(SubKey, sRemoteUrl, '');
      ListView1.Items.BeginUpdate;
      try
        ListItem := ListView1.Items.Add;
        ListItem.Caption := SubKey;
        ListItem.SubItems.Add(RemoteUrl);
      finally
        ListView1.Items.EndUpdate;
      end;
    end;
  finally
    RegIniFile.Free;
  end;
end;

end.
