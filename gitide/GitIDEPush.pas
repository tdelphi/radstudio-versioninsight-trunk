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
unit GitIDEPush;

interface

uses Classes, GitIDEMessageView, GitIDEMenus, GitClient, GitIDEClient;

type
  TBasePushGitMenu = class(TGitMenu)
  protected
    FGitIDEClient: TGitIDEClient;
    FRootType: TRootType;
    procedure Execute(const MenuContextList: IInterfaceList); override;
  public
    constructor Create(AGitIDEClient: TGitIDEClient);
  end;

  TParentPushGitMenu = class(TGitMenu)
  protected
    function GetImageIndex: Integer; override;
  public
    constructor Create;
  end;

  TRootDirPushGitMenu = class(TBasePushGitMenu)
  public
    constructor Create(AGitIDEClient: TGitIDEClient);
  end;

  TProjectDirPushGitMenu = class(TBasePushGitMenu)
  public
    constructor Create(AGitIDEClient: TGitIDEClient);
  end;

  TDirPushGitMenu = class(TBasePushGitMenu)
  protected
    function GetImageIndex: Integer; override;
  public
    constructor Create(AGitIDEClient: TGitIDEClient);
  end;

procedure DoPush(const GitClient: TGitClient; const ProjectDir: string);

implementation

uses SysUtils, GitIDEConst, ToolsApi, Controls, Forms, Dialogs, GitIDEIcons;

const
  sPMVPushParent = 'GitPushParent';
  sPMVRootDirPush = 'RootDirPush';
  sPMVProjectDirPush = 'ProjectDirPush';
  sPMVDirPush = 'DirPush';

{ TBaseCleanSvnMenu }

constructor TBasePushGitMenu.Create(AGitIDEClient: TGitIDEClient);
begin
  inherited;
  FParent := sPMVPushParent;
  FGitIDEClient := AGitIDEClient;
end;

procedure TBasePushGitMenu.Execute(const MenuContextList: IInterfaceList);
var
  Module: IOTAModule;
  I: Integer;
  Project: IOTAProject;
  ProjectDir: string;
begin
  if FGitIDEClient.GitClient.GitExecutable <> '' then
  begin
    for I := 0 to MenuContextList.Count - 1 do
    begin
      Project := (MenuContextList[I] as IOTAProjectMenuContext).Project;
      if Project <> nil then
        ProjectDir := ExtractFilePath(Project.FileName)
      else
      begin
        Module := (BorlandIDEServices as IOTAModuleServices).FindModule((MenuContextList[I] as IOTAMenuContext).Ident);
        if Module <> nil then
          ProjectDir := ExtractFilePath(Module.FileName);
      end;
    end;
    DoPush(FGitIDEClient.GitClient, ProjectDir);
  end
  else
    MessageDlg(sGitClientNotConfigured, mtError, [mbOk,mbHelp], hcGitClone);
end;

procedure DoPush(const GitClient: TGitClient; const ProjectDir: string);
var
 Cursor: TCursor;
 Output: string;
 OutputStrings: TStringList;
 I: Integer;
begin
  Cursor := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  OutputStrings := TStringList.Create;
  try
    SvnMessageView.CheckMessageGroup(True);
    SvnMessageView.WriteTitle(sPushing + ' from project directory ' + ProjectDir);
    GitClient.Push(ProjectDir, Output);
    OutputStrings.Text := Output;
    for I := 0 to OutputStrings.Count - 1 do
      SvnMessageView.WriteTitle(OutputStrings[I]);
  finally
    Screen.Cursor := Cursor;
    OutputStrings.Free;
  end;
end;

{ TParentCleanSvnMenu }

constructor TParentPushGitMenu.Create;
begin
  inherited Create(nil);
  FCaption := sPMMPush;
  FVerb := sPMVPushParent;
  FParent := sPMVGitParent;
  FPosition := pmmpParentCleanSvnMenu;
  FHelpContext := 0;
end;

function TParentPushGitMenu.GetImageIndex: Integer;
begin
  Result := CleanImageIndex;
end;

{ TRootDirCleanSvnMenu }

constructor TRootDirPushGitMenu.Create(AGitIDEClient: TGitIDEClient);
begin
  inherited Create(AGitIDEClient);
  FRootType := rtProjectDir;
  FCaption := sPMMRootDir;
  FVerb := sPMVRootDirPush;
  FPosition := pmmpRootDirPushSvnMenu;
  FHelpContext := 0;
end;

{ TProjectDirCleanSvnMenu }

constructor TProjectDirPushGitMenu.Create(AGitIDEClient: TGitIDEClient);
begin
  inherited Create(AGitIDEClient);
  FRootType := rtProjectDir;
  FCaption := sPMMProjectDir;
  FVerb := sPMVProjectDirPush;
  FPosition := pmmpProjectDirPushSvnMenu;
  FHelpContext := 0;
end;

{ TDirCleanSvnMenu }

constructor TDirPushGitMenu.Create(AGitIDEClient: TGitIDEClient);
begin
  inherited Create(AGitIDEClient);
  FRootType := rtDir;
  FParent := sPMVGitParent;
  FCaption := sPMMPush;
  FVerb := sPMVDirPush;
  FPosition := pmmpProjectDirPushSvnMenu;
  FHelpContext := 0;
end;

function TDirPushGitMenu.GetImageIndex: Integer;
begin
  Result := CleanImageIndex;
end;

end.
