{******************************************************************************}
{                                                                              }
{ RAD Studio Version Insight                                                   }
{                                                                              }
{ The contents of this file are subject to the Mozilla Public License          }
{ Version 1.1 (the "License"); you may not use this file except in compliance  }
{ with the License. You may obtain a copy of the License at                    }
{ http://www.mozilla.org/MPL/                                                  }
{                                                                              }
{ Software distributed under the License is distributed on an "AS IS" basis,   }
{ WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for }
{ the specific language governing rights and limitations under the License.    }
{                                                                              }
{ The Original Code is delphisvn: Subversion plugin for CodeGear Delphi.       }
{                                                                              }
{ The Initial Developer of the Original Code is Embarcadero Technologies.      }
{ Portions created by Ondrej Kelle are Copyright Ondrej Kelle. All rights      }
{ reserved.                                                                    }
{                                                                              }
{ Portions created or modified by Embarcadero Technologies are                 }
{ Copyright © 2010 Embarcadero Technologies, Inc. All Rights Reserved          }
{ Modifications include a major re-write of delphisvn. New functionality for   }
{ diffing, international character support, asynchronous gathering of data,    }
{ check-out and import, usability, tighter integration into RAD Studio, and    }
{ other new features.  Most original source files not used or re-written.      }
{                                                                              }
{ Contributors:                                                                }
{ Ondrej Kelle (tondrej)                                                       }
{ Uwe Schuster (uschuster)                                                     }
{ Embarcadero Technologies                                                     }
{                                                                              }
{******************************************************************************}
unit GitIDEImport;

interface

uses GitIDEClient, ToolsApi;

  function ImportProject(const AGitIDEClient: TGitIDEClient;
    const Project: IOTAProject): Boolean;

  procedure Register;

implementation

uses GitClientImportFrame, DesignIntf, Forms, Classes, SysUtils, GitIDEMenus,
  GitIDEConst, Dialogs, GitIDECommit, GitIDEUtils, GitUITypes{, GitClientRepoBrowserDialog};

const
  sGitImportView = 'GitImportView';

var
  ImportView: INTACustomEditorView;

type
  TImport = class(TInterfacedObject, INTACustomEditorView, INTACustomEditorView150)
  protected
    FGitIDEClient: TGitIDEClient;
    FGitImportFrame: TGitImportFrame;
    FSuggestedRepoPath: string;
    FRootPath: string;
    FCommitFiles: TStringList;
    FOutOfScopeFiles: TStringList;
    FNewLocation: Boolean;
    FVersionedRoot: Boolean;
    { INTACustomEditorView }
    function GetCanCloneView: Boolean;
    function CloneEditorView: INTACustomEditorView;
    function GetCaption: string;
    function GetEditorWindowCaption: string;
    function GetViewIdentifier: string;
    function GetEditState: TEditState;
    function EditAction(Action: TEditAction): Boolean;
    procedure CloseAllCalled(var ShouldClose: Boolean);
    procedure SelectView;
    procedure DeselectView;
    function GetFrameClass: TCustomFrameClass;
    procedure FrameCreated(AFrame: TCustomFrame);
    { INTACustomEditorView150 }
    function GetImageIndex: Integer;
    function GetTabHintText: string;
    procedure Close(var Allowed: Boolean);
    { CallBacks }
    function ImportCallBack(const RepoPath, Comment: string;
      const RecentComments: TStringList; const URLHistory: TStringList): Boolean;
    procedure CloseCallBack;
    function BrowseURLCallBack(var AURL: string): Boolean;
    { Misc }
    function ImportNewLocation(const RepoPath, Comment: string;
      const RecentComments: TStringList): Boolean;
    function ImportExisting(const RepoPath, Comment: string;
      const RecentComments: TStringList): Boolean;
  public
    constructor Create(AGitIDEClient: TGitIDEClient;
      const SuggestedRepoPath, RootPath: string;
      const CommitFiles, OutOfScopeFiles: TStringList; NewLocation: Boolean;
      VersionedRoot: Boolean);
    destructor Destroy; override;
  end;

function ImportProject(const AGitIDEClient: TGitIDEClient;
  const Project: IOTAProject): Boolean;
var
  SuggestedRepoPath: string;
  Path: string;
  RootPath: string;
  I: Integer;
  OutofScopeFiles: TStringList;
  FileList: TStringList;
  CommitFiles: TStringList;
  NewLocation: Boolean;
  VersionedRoot: Boolean;

  // Only look in currect path for a versioned path and one directory up
  function ManagedLocation(var Path: string; var SameDirectory: Boolean): string;
  var
    S: string;
  begin
    Result := '';
    if IDEClient.GitClient.IsPathInWorkingCopy(Path) then
    begin
      Result := AGitIDEClient.GitClient.FindRepositoryRoot(Path);
      VersionedRoot := True;
    end
    else
    begin
      VersionedRoot := False;
      S := ExcludeTrailingPathDelimiter(Path);
      Path := ExtractFilePath(S);
      S := ExtractFileName(S);
      if IDEClient.GitClient.IsPathInWorkingCopy(Path) then
        Result := AGitIDEClient.GitClient.FindRepositoryRoot(Path) + '/' + S;
    end;
  end;

begin
  if SaveAll then
  begin
    Path := ExtractFilePath(Project.FileName);
    SuggestedRepoPath := ManagedLocation(Path, VersionedRoot);
    NewLocation := SuggestedRepoPath = '';
    if NewLocation then
      RootPath := ExtractFilePath(Project.FileName)
    else
      RootPath := RootDirectory(AGitIDEClient.GitClient, Path);
    RootPath := AnsiLowerCase(RootPath);
    CommitFiles := TStringList.Create;
    try
      OutofScopeFiles := TStringList.Create;
      try
        FileList := TStringList.Create;
        try
          Project.GetCompleteFileList(FileList);
          for I := 0 to FileList.Count - 1 do
            if Pos(RootPath, AnsiLowerCase(FileList[I])) <> 1 then
              OutOfScopeFiles.Add(FileList[I])
            else
              CommitFiles.Add(FileList[I]);
          ImportView := TImport.Create(AGitIDEClient, SuggestedRepoPath, RootPath,
            CommitFiles, OutOfScopeFiles, NewLocation, VersionedRoot);
          (BorlandIDEServices as IOTAEditorViewServices).ShowEditorView(ImportView);
        finally
          FileList.Free;
        end;
      finally
        OutofScopeFiles.Free;
      end;
    finally
      CommitFiles.Free;
    end;
    Result := True;
  end
  else
    Result := False;
end;

{ TImport }

function TImport.BrowseURLCallBack(var AURL: string): Boolean;
begin
  Result := False;
  //Result := GetRepoURL(FGitIDEClient.GitClient, AURL);
end;

function TImport.CloneEditorView: INTACustomEditorView;
begin
  Result := nil;
end;

procedure TImport.Close(var Allowed: Boolean);
begin
  Allowed := True;
  ImportView := nil;
end;

procedure TImport.CloseAllCalled(var ShouldClose: Boolean);
begin
  ShouldClose := True;
end;

procedure TImport.CloseCallBack;
begin
  (BorlandIDEServices as IOTAEditorViewServices).CloseActiveEditorView;
end;

constructor TImport.Create(AGitIDEClient: TGitIDEClient;
      const SuggestedRepoPath, RootPath: string; 
      const CommitFiles, OutOfScopeFiles: TStringList;
      NewLocation: Boolean; VersionedRoot: Boolean);
begin
  inherited Create;
  FGitIDEClient := AGitIDEClient;
  FSuggestedRepoPath := SuggestedRepoPath;
  FRootPath := RootPath;
  FNewLocation := NewLocation;
  FVersionedRoot := VersionedRoot;
  FOutOfScopeFiles := TStringList.Create;
  FOutOfScopeFiles.Assign(OutOfScopeFiles);
  FCommitFiles := TStringList.Create;
  FCommitFiles.Assign(CommitFiles);
  SaveAll;
end;

procedure TImport.DeselectView;
begin
  // Not used
end;

destructor TImport.Destroy;
begin
  FOutOfScopeFiles.Free;
  FCommitFiles.Free;
  inherited;
end;

function TImport.EditAction(Action: TEditAction): Boolean;
var
  GitEditAction: TSvnEditAction;
begin
  Result := False;
  if Assigned(FGitImportFrame) then
  begin
    GitEditAction := EditActionToSvnEditAction(Action);
    if GitEditAction <> seaUnknown then
      Result := FGitImportFrame.PerformEditAction(GitEditAction);
  end;
end;

procedure TImport.FrameCreated(AFrame: TCustomFrame);
var
  RecentComments: TStringList;
  URLHistory: TStringList;
begin
  FGitImportFrame := TGitImportFrame(AFrame);
  if FSuggestedRepoPath <> '' then
  begin
    FGitImportFrame.URL.Text := FSuggestedRepoPath;
    FGitImportFrame.UpdateImportButton;
  end;
  FGitImportFrame.VersionedRoot := FVersionedRoot;
  FGitImportFrame.OutOfScopeFilesGroupBox.Caption :=
    Format(FGitImportFrame.OutOfScopeFilesGroupBox.Caption, [FRootPath]);
  FGitImportFrame.SetFiles(FCommitFiles);
  FGitImportFrame.OutOfScopeFiles.Items.Assign(FOutOfScopeFiles);
  FGitImportFrame.ImportCallBack := ImportCallBack;
  FGitImportFrame.CloseCallBack := CloseCallBack;
    FGitImportFrame.BrowseURLCallBack := BrowseURLCallBack;
  RecentComments := TStringList.Create;
  try
    LoadRecentComments(RecentComments);
    FGitImportFrame.RecentComments := RecentComments;
  finally
    RecentComments.Free;
  end;
  URLHistory := TStringList.Create;
  try
    LoadSourceRepoHistory(URLHistory);
    FGitImportFrame.URLHistory := URLHistory;
  finally
    URLHistory.Free;
  end;
end;

function TImport.GetCanCloneView: Boolean;
begin
  Result := False;
end;

function TImport.GetCaption: string;
begin
  Result := sImport;
end;

function TImport.GetEditorWindowCaption: string;
begin
  Result := sImport;
end;

function TImport.GetEditState: TEditState;
begin
  Result := [];
  if Assigned(FGitImportFrame) then
    Result := SvnEditStateToEditState(FGitImportFrame.SvnEditState);
end;

function TImport.GetFrameClass: TCustomFrameClass;
begin
  Result := TGitImportFrame;
end;

function TImport.GetImageIndex: Integer;
begin
  Result := 0;
end;

function TImport.GetTabHintText: string;
begin
  Result := '';
  //TODO: provide a custom hint for the Import tab
end;

function TImport.GetViewIdentifier: string;
begin
  Result := sGitImportView;
end;

function TImport.ImportCallBack(const RepoPath, Comment: string;
  const RecentComments: TStringList; const URLHistory: TStringList): Boolean;
begin
  try
    SaveSourceRepoHistory(URLHistory);
    FCommitFiles.Clear;
    FGitImportFrame.GetFiles(FCommitFiles);
    if FNewLocation then
      Result := ImportNewLocation(RepoPath, Comment, RecentComments)
    else
      Result := ImportExisting(RepoPath, Comment, RecentComments);
  except
    on E: Exception do
    begin
      ShowMessage(E.ClassName+' error raised, with message : '+E.Message);
      Result := False;
    end;
  end;
end;

function TImport.ImportExisting(const RepoPath, Comment: string;
  const RecentComments: TStringList): Boolean;
var
  I: Integer;
  S: string;
  DirectoryList: TStringList;
  CheckInList: TStringList;
begin
  Result := True;
  DirectoryList := TStringList.Create;
  DirectoryList.Sorted := True;
  DirectoryList.CaseSensitive := False;
  DirectoryList.Add(ExcludeTrailingPathDelimiter(FRootPath));
  try
    for I := 0 to FCommitFiles.Count - 1 do
    begin
      if FCommitFiles[I] <> '' then
      begin
        S := ExcludeTrailingPathDelimiter(ExtractFilePath(FCommitFiles[I]));
        while (DirectoryList.IndexOf(S) = -1) and (S <> '') do
        begin
          DirectoryList.Add(S);
          S := ExcludeTrailingPathDelimiter(ExtractFilePath(S));
        end;
      end;
    end;
    CheckInList := TStringList.Create;
    try
      for I := 0 to DirectoryList.Count - 1 do
        if not IDEClient.GitClient.IsPathInWorkingCopy(DirectoryList[I]) then
        begin
          IDEClient.GitClient.Add(DirectoryList[I]);
          CheckInList.Add(DirectoryList[I]);
        end;
      try
        for I := 0 to FCommitFiles.Count - 1 do
          IDEClient.GitClient.Add(FCommitFiles[I]);
      except
        on E : Exception do
        begin
          ShowMessage(E.ClassName+' error raised, with message : '+E.Message);
          Result := False;
        end;
      end;
      for I := 0 to FCommitFiles.Count - 1 do
      begin
        CheckInList.Add(FCommitFiles[I]);
        FGitIDEClient.GitClient.Add(FCommitFiles[I]);
      end;
      DoCommit(FGitIDEClient.GitClient, CheckInList, Comment, RecentComments,
        FGitIDEClient.Options.DeleteBackupFilesAfterCommit);
    finally
      CheckInList.Free;
    end;
  finally
    DirectoryList.Free;
  end;
end;

function TImport.ImportNewLocation(const RepoPath, Comment: string;
  const RecentComments: TStringList): Boolean;
var
  Paths: TStringList;
  LRepoPath: string;
begin
  Paths := TStringList.Create;
  try
    LRepoPath := RepoPath;
    if IsDelimiter('/\', LRepoPath, Length(LRepoPath)) then
    begin
      SetLength(LRepoPath, Length(LRepoPath) - 1);
    end;
    Paths.Add(LRepoPath);
    try
      IDEClient.GitClient.InitializeRepository(FRootPath, RepoPath);
    except
      on E : Exception do
        ShowMessage(E.ClassName+' error raised, with message : '+E.Message);
    end;
  finally
    Paths.Free;
  end;
  Result := ImportExisting(LRepoPath, Comment, RecentComments);
end;

procedure TImport.SelectView;
begin
  // Not used
end;

function GetView: INTACustomEditorView;
begin
  Result := ImportView;
end;

procedure Register;
begin
  (BorlandIDEServices as IOTAEditorViewServices).RegisterEditorView(sGitImportView, GetView);
end;

initialization
finalization
  (BorlandIDEServices as IOTAEditorViewServices).UnRegisterEditorView(sGitImportView);
end.
