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
{ The Original Code is GitAddInOptionsFrame.pas.                               }
{                                                                              }
{ The Initial Developer of the Original Code is Uwe Schuster.                  }
{ Portions created by Uwe Schuster are Copyright © 2010 - 2014 Uwe Schuster.   }
{ All Rights Reserved.                                                         }
{                                                                              }
{ Contributors:                                                                }
{ Uwe Schuster (uschuster)                                                     }
{                                                                              }
{******************************************************************************}

unit GitAddInOptionsFrame;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, StdCtrls, Buttons, ExtCtrls;

type
  TfrmGitTestsOptions = class(TFrame)
    GroupBox1: TGroupBox;
    edGitExecutable: TEdit;
    Label1: TLabel;
    OpenDialog1: TOpenDialog;
    SpeedButton1: TSpeedButton;
    GroupBox2: TGroupBox;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    cboxConflicted: TColorBox;
    cboxAdded: TColorBox;
    cboxDeleted: TColorBox;
    cboxMerged: TColorBox;
    cboxModified: TColorBox;
    cbStatusColorsEnabled: TCheckBox;
    GroupBox3: TGroupBox;
    cbDeleteBackupFilesAfterCommit: TCheckBox;
    cbAlternativeCommitLayout: TCheckBox;
    cbClearFileStatesAfterCloseAll: TCheckBox;
    cbKeepCommitViewOpenAfterCommit: TCheckBox;
    btnClear: TButton;
    lbAuthentication: TLabel;
    btnClearAll: TButton;
    lbUserName: TLabel;
    lbEmail: TLabel;
    edUserName: TEdit;
    edUserEmail: TEdit;
    Label7: TLabel;
    procedure SpeedButton1Click(Sender: TObject);
    procedure btnClearClick(Sender: TObject);
    procedure btnClearAllClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

uses GitIDESavedRepoInfo, System.Win.Registry, ToolsApi, GitUIConst, GitIDEConst;

{$R *.dfm}

procedure TfrmGitTestsOptions.btnClearAllClick(Sender: TObject);
var
  RegIniFile: TRegIniFile;
  SubKeyNames: TStringList;
  Key: string;
  SubKey: string;
begin
  if MessageDlg(sConfirmRemoveRemoteInfo, mtConfirmation, mbYesNo, 0) = mrYes then
  begin
    Key := (BorlandIDEServices as IOTAServices).GetBaseRegistryKey + sGitRegBaseKey;
    RegIniFile := TRegIniFile.Create(Key);
    SubKeyNames := TStringList.Create;
    try
      RegIniFile.GetKeyNames(SubKeyNames);
      for SubKey in SubKeyNames do
        RegIniFile.EraseSection(SubKey);
    finally
      RegIniFile.Free;
    end;
  end;

end;

procedure TfrmGitTestsOptions.btnClearClick(Sender: TObject);
begin
  ShowGitSavedRepoInfo;
end;

procedure TfrmGitTestsOptions.SpeedButton1Click(Sender: TObject);
begin
  if OpenDialog1.Execute then
    edGitExecutable.Text := OpenDialog1.FileName;
end;

end.
