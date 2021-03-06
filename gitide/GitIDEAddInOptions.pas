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
{ The Original Code is GitIDEAddInOptions.pas.                                 }
{                                                                              }
{ The Initial Developer of the Original Code is Uwe Schuster.                  }
{ Portions created by Uwe Schuster are Copyright � 2010 - 2014 Uwe Schuster.   }
{ All Rights Reserved.                                                         }
{                                                                              }
{ Contributors:                                                                }
{ Uwe Schuster (uschuster)                                                     }
{                                                                              }
{******************************************************************************}

unit GitIDEAddInOptions;

interface

uses
  Forms, ToolsAPI, GitAddInOptionsFrame;

type
  TGitAddInOptions = class(TInterfacedObject, INTAAddInOptions)
  private
    FFrame: TfrmGitTestsOptions;
  public
    procedure DialogClosed(Accepted: Boolean);
    procedure FrameCreated(AFrame: TCustomFrame);
    function GetArea: string;
    function GetCaption: string;
    function GetFrameClass: TCustomFrameClass;
    function GetHelpContext: Integer;
    function ValidateContents: Boolean;
    function IncludeInIDEInsight: Boolean;
  end;

procedure RegisterAddInOptions;

implementation

uses
  Registry, GitIDEClient, GitIDEColors, GitIDEConst;

{ TGitAddInOptions }

procedure TGitAddInOptions.DialogClosed(Accepted: Boolean);
var
  RegIniFile: TRegIniFile;
  Key: string;
  Colors: TSvnColorArray;
begin
  if Accepted then
  begin
    if (IDEClient.GitClient.GitExecutable <> FFrame.edGitExecutable.Text)
      or (IDEClient.GitClient.UserName <> FFrame.edUserName.Text)
      or (IDEClient.GitClient.UserEmail <> FFrame.edUserEmail.Text) then
    begin
      IDEClient.GitClient.GitExecutable := FFrame.edGitExecutable.Text;
      IDEClient.GitClient.UserName := FFrame.edUserName.Text;
      IDEClient.GitClient.UserEmail := FFrame.edUserEmail.Text;

      Key := (BorlandIDEServices as IOTAServices).GetBaseRegistryKey + '\VersionInsight';
      RegIniFile := TRegIniFile.Create(Key);
      try
        RegIniFile.WriteString('Git', 'Executable', IDEClient.GitClient.GitExecutable);
        RegIniFile.WriteString('Git', 'UserName', IDEClient.GitClient.UserName);
        RegIniFile.WriteString('Git', 'UserEmail', IDEClient.GitClient.UserEmail);
      finally
        RegIniFile.Free;
      end;
    end;
    Colors := IDEClient.Colors.Colors;
    Colors[ssckConflicted] := FFrame.cboxConflicted.Selected;
    Colors[ssckAdded] := FFrame.cboxAdded.Selected;
    Colors[ssckDeleted] := FFrame.cboxDeleted.Selected;
    Colors[ssckMerged] := FFrame.cboxMerged.Selected;
    Colors[ssckModified] := FFrame.cboxModified.Selected;
    IDEClient.Colors.Colors := Colors;
    IDEClient.Colors.Save;
    IDEClient.Options.AlternativeCommitLayout := FFrame.cbAlternativeCommitLayout.Checked;
    IDEClient.Options.ClearFileStatesAfterCloseAll := FFrame.cbClearFileStatesAfterCloseAll.Checked;
    IDEClient.Options.DeleteBackupFilesAfterCommit := FFrame.cbDeleteBackupFilesAfterCommit.Checked;
    IDEClient.Options.KeepCommitViewOpenAfterCommit := FFrame.cbKeepCommitViewOpenAfterCommit.Checked;
    IDEClient.Options.Save;
  end;
end;

procedure TGitAddInOptions.FrameCreated(AFrame: TCustomFrame);
begin
  FFrame := TfrmGitTestsOptions(AFrame);
  FFrame.edGitExecutable.Text := IDEClient.GitClient.GitExecutable;
  FFrame.edUserName.Text := IDEClient.GitClient.UserName;
  FFrame.edUserEmail.Text := IDEClient.GitClient.UserEmail;
  FFrame.cbStatusColorsEnabled.Checked := IDEClient.Colors.Enabled;
  FFrame.cboxConflicted.Selected := IDEClient.Colors.Colors[ssckConflicted];
  FFrame.cboxAdded.Selected := IDEClient.Colors.Colors[ssckAdded];
  FFrame.cboxDeleted.Selected := IDEClient.Colors.Colors[ssckDeleted];
  FFrame.cboxMerged.Selected := IDEClient.Colors.Colors[ssckMerged];
  FFrame.cboxModified.Selected := IDEClient.Colors.Colors[ssckModified];
  FFrame.cbAlternativeCommitLayout.Checked := IDEClient.Options.AlternativeCommitLayout;
  {$IFNDEF TOOLSPROAPI}
  FFrame.cbClearFileStatesAfterCloseAll.Visible := False;
  {$ENDIF ~TOOLSPROAPI}
  FFrame.cbClearFileStatesAfterCloseAll.Checked := IDEClient.Options.ClearFileStatesAfterCloseAll;
  FFrame.cbDeleteBackupFilesAfterCommit.Checked := IDEClient.Options.DeleteBackupFilesAfterCommit;
  FFrame.cbKeepCommitViewOpenAfterCommit.Checked := IDEClient.Options.KeepCommitViewOpenAfterCommit;
end;

function TGitAddInOptions.GetArea: string;
begin
  Result := sVersionControlAddInOptionArea;
end;

function TGitAddInOptions.GetCaption: string;
begin
  Result := sGit;
end;

function TGitAddInOptions.GetFrameClass: TCustomFrameClass;
begin
  Result := TfrmGitTestsOptions;
end;

function TGitAddInOptions.GetHelpContext: Integer;
begin
  Result := hcGitOptions;
end;

function TGitAddInOptions.IncludeInIDEInsight: Boolean;
begin
  Result := True;
end;

function TGitAddInOptions.ValidateContents: Boolean;
begin
  Result := True;
end;

var
  GitAddInOptions: TGitAddInOptions = nil;

procedure RegisterAddInOptions;
begin
  GitAddInOptions := TGitAddInOptions.Create;
  (BorlandIDEServices as INTAEnvironmentOptionsServices).RegisterAddInOptions(GitAddInOptions);
end;

initialization

finalization
  (BorlandIDEServices as INTAEnvironmentOptionsServices).UnregisterAddInOptions(GitAddInOptions);
  GitAddInOptions := nil;

end.
