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
unit GitIDECheckout;

interface

function DoCheckOutProject(var ProjectName: string; const Connection: string = ''): Boolean;

implementation

uses {SvnClient, }GitIDEClient, GitClientCheckout, {SvnIDEMessageView, }ToolsApi,
  Classes, GitClientProjectSelect, SysUtils, {SvnClientRepoBrowserDialog,}
  GitClientUpdate, GitIDEConst, Generics.Collections, {SvnIDEConst, }Graphics,
  Dialogs, Vcl.Forms, Vcl.Controls;

type
  TCheckoutThread = class(TThread)
  protected
    FRemoteUrl: string;
    FTargetDir: string;
    FSyncPath: string;
    FSyncAction: string;
    FSyncTextColor: TColor;
    FUpdateDialog: TGitUpdateDialog;
    FExceptionMessage: string;
    FProjectNames: TStringList;
    FProjectGroupNames: TStringList;
    FProjectName: string;
    FAborted: Boolean;
    procedure AbortCallBack;
    procedure Add(const Path, Action: string; TextColor: TColor);
    procedure SearchProjects;
    procedure SyncAdd;
    procedure SyncCompleted;
    procedure Execute; override;
    procedure CloneCallBack(Sender: TObject; const AText: string; var Cancel: Boolean);
    procedure OpenProject;
  public
    constructor Create(const RemoteUrl, TargetDir: string); reintroduce;
    destructor Destroy; override;
  end;

function DoCheckout(const RemoteUrl, TargetDir: string; const ProjectName: string): string;
var
  RemoteStatus: string;
  FormattedUrl: string;

  function CheckRemoteStatus(const AUrl: string): Boolean;
  var
    Cursor: Smallint;
  begin
    Cursor := Screen.Cursor;
    Screen.Cursor := crHourGlass;
    Result := IDEClient.GitClient.CheckRemoteStatus(TargetDir, AUrl, RemoteStatus);
    Screen.Cursor := Cursor;
  end;

begin
  Result := '';
  if IDEClient.GitClient.GitExecutable <> '' then
  begin
    //Close Active Projects
    if (BorlandIDEServices as IOTAModuleServices).MainProjectGroup <> nil then
      (BorlandIDEServices as IOTAModuleServices).MainProjectGroup.Close;

    if CheckRemoteStatus(RemoteUrl) then
      TCheckoutThread.Create(RemoteUrl, TargetDir)
    else
    begin
      if RemoteStatus = '' then
      begin
        IDEClient.GitClient.EnsureUserAuthentication(TargetDir, RemoteUrl);
        if IDEClient.GitClient.UserAuthenticated then
        begin
          FormattedUrl := IDEClient.GitClient.FormatUrl(RemoteUrl, IDEClient.GitClient.RemoteUserName,
            IDEClient.GitClient.RemoteUserPass);
          if CheckRemoteStatus(FormattedUrl) then
            TCheckoutThread.Create(RemoteUrl, TargetDir)
          else
            MessageDlg(RemoteStatus.Replace(FormattedUrl, RemoteUrl), mtError, [mbOk, mbHelp], hcGitClone);
        end;
      end
      else
        MessageDlg(RemoteStatus.Replace(FormattedUrl, RemoteUrl), mtError, [mbOk, mbHelp], hcGitClone);
    end;
  end
  else
    MessageDlg(sGitClientNotConfigured, mtError, [mbOk,mbHelp], hcGitClone);
end;

function DoCheckOutProject(var ProjectName: string; const Connection: string): Boolean;
var
  RemoteUrl: string;
  TargetDir: string;
  InitalDirectory: string;
  URLHistory: TStringList;
  Loc: Integer;
begin
  InitalDirectory := (BorlandIDEServices as IOTAServices).GetStartupDirectory;
  URLHistory := TStringList.Create;
  try
    LoadSourceRepoHistory(URLHistory);
    Loc := Pos(';', Connection);
    if Loc = 0  then
      RemoteUrl := Connection
    else
    begin
      RemoteUrl := Copy(Connection, 1, Loc - 1);
      ProjectName := Copy(Connection, Loc + 1, MaxInt);
    end;
    Result := GetCheckoutInformation(URLHistory, InitalDirectory, RemoteUrl, TargetDir);
    if Result then
      SaveSourceRepoHistory(URLHistory);
  finally
    URLHistory.Free;
  end;
  if Result then
   ProjectName := DoCheckout(RemoteUrl, TargetDir, ProjectName);
end;

{ TCheckoutThread }

procedure TCheckoutThread.AbortCallBack;
begin
  FAborted := True;
end;

procedure TCheckoutThread.Add(const Path, Action: string; TextColor: TColor);
begin
  FSyncPath := Path;
  FSyncAction := Action;
  FSyncTextColor := TextColor;
  Synchronize(nil, SyncAdd);
end;

procedure TCheckoutThread.CloneCallBack(Sender: TObject; const AText: string;
  var Cancel: Boolean);
var
  Action: string;
  P: Integer;
begin
  Action := AText;
  P := Pos(#$1B + '[K', Action);
  if P > 0 then
    Delete(Action, P, MaxInt);
  Add('', Action, clNone);
  Cancel := FAborted;
end;

constructor TCheckoutThread.Create(const RemoteUrl, TargetDir: string);
begin
  inherited Create(True);
  FRemoteUrl := RemoteUrl;
  FTargetDir := TargetDir;
  FUpdateDialog := GetUpdateDialog(RemoteUrl, AbortCallBack, nil, nil);
  FUpdateDialog.Caption := Format('Clone - %s', [RemoteUrl]);
  FUpdateDialog.Show;
  FProjectNames := TStringList.Create;
  FProjectGroupNames := TStringList.Create;
  FAborted := False;
  FreeOnTerminate := True;
  Resume;
end;

destructor TCheckoutThread.Destroy;
begin
  FProjectNames.Free;
  FProjectGroupNames.Free;
  inherited;
end;

procedure EliminateChildProjectFiles(AProjectNames: TStringList);
var
  I: Integer;
  ParentExt: string;
  ParentFileDictionary: TDictionary<string, string>;
begin
  if AProjectNames.Count > 1 then
  begin
    AProjectNames.Sorted := True;
    ParentFileDictionary := TDictionary<string, string>.Create;
    try
      //Delphi mappings
      ParentFileDictionary.Add('.dpr', '.dproj');
      ParentFileDictionary.Add('.dpk', '.dproj');
      for I := AProjectNames.Count - 1 downto 0 do
        if ParentFileDictionary.TryGetValue(AnsiLowerCase(ExtractFileExt(AProjectNames[I])), ParentExt) and
          (AProjectNames.IndexOf(ChangeFileExt(AProjectNames[I], ParentExt)) <> -1) then
          AProjectNames.Delete(I);
    finally
      ParentFileDictionary.Free;
    end;
  end;
end;

procedure TCheckoutThread.Execute;
begin
  NameThreadForDebugging('VerIns Git Clone');
  try
    FExceptionMessage := '';
    IDEClient.GitClient.Clone(FRemoteUrl, FTargetDir, CloneCallBack);
  except
    {
    if not GetSvnExceptionMessage(ExceptObject, FExceptionMessage) then
      raise;
    }
  end;
  Synchronize(nil, SyncCompleted);
end;

procedure TCheckoutThread.OpenProject;
begin
  if FProjectName = '' then
    SelectProject(FProjectName, FProjectNames, FProjectGroupNames)
  else
  begin
    FProjectName := IncludeTrailingPathDelimiter(FTargetDir) + FProjectName;
  end;
  if FProjectName <> '' then
    (BorlandIDEServices as IOTAModuleServices).OpenModule(FProjectName);
end;

procedure TCheckoutThread.SearchProjects;

  procedure FindProjects(const APath: string);
  var
    F: TSearchRec;
    R: Integer;
    FileName: string;
  begin
    R := FindFirst(IncludeTrailingPathDelimiter(APath) + '*.*', faAnyFile, F);
    try
      while R = 0 do
      begin
        if (F.Name <> '.') and (F.Name <> '..') then
        begin
          if ((F.Attr and faDirectory) <> 0) and not SameText(F.Name, '.git') then
            FindProjects(IncludeTrailingPathDelimiter(APath) + F.Name)
          else
          begin
            FileName := IncludeTrailingPathDelimiter(APath) + F.Name;
            if (BorlandIDEServices as IOTAServices).IsProject(FileName) then
              FProjectNames.Add(FileName);
            if (BorlandIDEServices as IOTAServices).IsProjectGroup(FileName) then
              FProjectGroupNames.Add(FileName);
          end;
        end;
        R := FindNext(F);
      end;
    finally
      FindClose(F);
    end;
  end;

begin
  FindProjects(FTargetDir);
end;

procedure TCheckoutThread.SyncAdd;
begin
  FUpdateDialog.Add(FSyncPath, FSyncAction, False, FSyncTextColor);
end;

procedure TCheckoutThread.SyncCompleted;
begin
  if FExceptionMessage <> '' then
    //ShowSvnExceptionMessage(FExceptionMessage)
  else
  begin
    SearchProjects;
    EliminateChildProjectFiles(FProjectNames);
    FProjectGroupNames.Sort;
    OpenProject;
  end;
  FUpdateDialog.Completed;
end;

end.
