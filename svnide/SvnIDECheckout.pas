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
unit SvnIDECheckout;

interface

uses svn_client, SvnIdeClient, Dialogs;

function DoCheckOutProject(var ProjectName: string; const Connection: string = ''): Boolean;

implementation

uses SvnClient, SvnClientCheckout, SvnIDEMessageView, ToolsApi,
  Classes, SvnClientProjectSelect, SysUtils, SvnClientRepoBrowserDialog,
  SvnClientUpdate, Generics.Collections, SvnIDEConst, Graphics;

type
  TCheckoutThread = class(TThread)
  protected
    FPathName: string;
    FTargetDir: string;
    FRecurse: Boolean;
    FIgnoreExternals: Boolean;
    FRevision: TSvnRevNum;
    FSyncPath: string;
    FSyncAction: string;
    FSyncTextColor: TColor;
    FUpdateDialog: TUpdateDialog;
    FExceptionMessage: string;
    FProjectNames: TStringList;
    FProjectGroupNames: TStringList;
    FProjectName: string;
    FAborted: Boolean;
    procedure AbortCallBack;
    procedure Add(const Path, Action: string; TextColor: TColor);
    procedure SyncAdd;
    procedure SyncCompleted;
    procedure Execute; override;
    procedure CancelCallback(Sender: TObject; var Cancel: Boolean);
    procedure CheckoutCallBack(Sender: TObject; const Path, MimeType: string;
      Action: TSvnWcNotifyAction; Kind: TSvnNodeKind; ContentState,
      PropState: TSvnWCNotifyState; Revision: TSvnRevNum; var Cancel: Boolean);
    procedure OpenProject;
  public
    constructor Create(const PathName, TargetDir: string;
      Recurse, IgnoreExternals: Boolean; Revision: TSvnRevNum); reintroduce;
    destructor Destroy; override;
  end;

resourcestring
  sInvalidUrl = 'Invalid url';

function DoCheckout(const PathName, TargetDir: string; Recurse: Boolean;
  IgnoreExternals: Boolean; Revision: TSvnRevNum; const ProjectName: string): string;
begin
  Result := '';
  TCheckoutThread.Create(PathName, TargetDir, Recurse, IgnoreExternals, Revision);
end;

function BrowseURLCallBack(var AURL: string): Boolean;
begin
  Result := GetRepoURL(SvnIDEClient.IDEClient.SvnClient, AURL);
end;

function DoCheckOutProject(var ProjectName: string; const Connection: string): Boolean;
var
  PathName: string;
  TargetDir: string;
  Recurse: Boolean;
  IgnoreExternals: Boolean;
  Revision: TSvnRevNum;
  InitalDirectory: string;
  URLHistory: TStringList;
  Loc: Integer;

  procedure FormatUrl;
  var
    I: Integer;
    StrList: TStringList;
    RootURLIndx: Integer;
  begin
    StrList := TStringList.Create;
    try
      StrList.StrictDelimiter := True;
      StrList.Delimiter := '/';
      StrList.DelimitedText := PathName;
      RootURLIndx := StrList.IndexOf('svn');
      if RootURLIndx = -1 then
        RootURLIndx := StrList.IndexOf('svnroot');
      for I := 0 to StrList.Count -1 do
      begin
        if StrList[I] = '' then Continue;
        if I = 0 then
          PathName := StrList[I] + '//'
        else if I <= RootURLIndx then
          PathName := PathName + LowerCase(StrList[I]) + StrList.Delimiter
        else
          PathName := PathName + StrList[I] + StrList.Delimiter;
      end;
    finally
      StrList.Free;
    end;
  end;
begin
  InitalDirectory := (BorlandIDEServices as IOTAServices).GetStartupDirectory;
  URLHistory := TStringList.Create;
  try
    LoadURLHistory(URLHistory);
    Loc := Pos(';', Connection);
    if Loc = 0  then
      PathName := Connection
    else
    begin
      PathName := Copy(Connection, 1, Loc - 1);
      ProjectName := Copy(Connection, Loc + 1, MaxInt);
    end;
    Result := GetCheckoutInformation(URLHistory, InitalDirectory, PathName, TargetDir, Recurse,
      IgnoreExternals, Revision, BrowseURLCallBack);
    if Result then
      SaveURLHistory(URLHistory);
  finally
    URLHistory.Free;
  end;
  if Result then
  begin
    if IDEClient.SvnClient.SvnPathIsUrl(PathName) then
    begin
      FormatUrl;
      if PathName[PathName.Length] = '/' then
        PathName := Copy(PathName, 0, PathName.Length -1);
    ProjectName := DoCheckout(PathName, TargetDir, Recurse, IgnoreExternals,
      Revision, ProjectName);
    end
    else
      MessageDlg(sInvalidUrl, mtError, [mbOk], 0);
  end;
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

procedure TCheckoutThread.CancelCallback(Sender: TObject; var Cancel: Boolean);
begin
  Cancel := FAborted;
end;

procedure TCheckoutThread.CheckoutCallBack(Sender: TObject; const Path,
  MimeType: string; Action: TSvnWcNotifyAction; Kind: TSvnNodeKind;
  ContentState, PropState: TSvnWCNotifyState; Revision: TSvnRevNum;
  var Cancel: Boolean);
var
  FileName: string;
  TextColor: TColor;
begin
  FileName := StringReplace(Path, '/', '\', [rfReplaceAll]);
  TextColor := IDEClient.Colors.GetNotifyActionColor(Action, ContentState);
  if Action = svnWcNotifyUpdateCompleted then
    Add(Format(sUpdateCompletedAtRevision, [Revision]), NotifyActionStr(Action), TextColor)
  else
    Add(FileName, NotifyActionStr(Action), TextColor);
  if (BorlandIDEServices as IOTAServices).IsProject(FileName) then
    FProjectNames.Add(FileName);
  if (BorlandIDEServices as IOTAServices).IsProjectGroup(FileName) then
    FProjectGroupNames.Add(FileName);
end;

constructor TCheckoutThread.Create(const PathName, TargetDir: string;
  Recurse, IgnoreExternals: Boolean; Revision: TSvnRevNum);
begin
  inherited Create(True);
  FPathName := PathName;
  FTargetDir := TargetDir;
  FRecurse := Recurse;
  FIgnoreExternals := IgnoreExternals;
  FRevision := Revision;
  FUpdateDialog := GetUpdateDialog(PathName, AbortCallBack, nil, nil);
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
  NameThreadForDebugging('DelphiSVN Checkout');
  try
    FExceptionMessage := '';
    IDEClient.SvnClient.Checkout(FPathName, FTargetDir, CheckoutCallBack,
      FRecurse, FIgnoreExternals, FRevision, -1, CancelCallback);
  except
    if not GetSvnExceptionMessage(ExceptObject, FExceptionMessage) then
      raise;
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

procedure TCheckoutThread.SyncAdd;
begin
  FUpdateDialog.Add(FSyncPath, FSyncAction, False, FSyncTextColor);
end;

procedure TCheckoutThread.SyncCompleted;
begin
  if FExceptionMessage <> '' then
    ShowSvnExceptionMessage(FExceptionMessage)
  else
  begin
    EliminateChildProjectFiles(FProjectNames);
    FProjectGroupNames.Sort;
    OpenProject;
  end;
  FUpdateDialog.Completed;
end;

end.
