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
unit GitIDEUpdate;

interface

uses Classes, ToolsApi, GitClient, GitIDEClient, GitIDEMenus;

type
  TBaseUpdateGitMenu = class(TGitMenu)
  protected
    FGitIDEClient: TGitIDEClient;
    FRootType: TRootType;
    { Misc }
    procedure Execute(const MenuContextList: IInterfaceList); override;
  public
    constructor Create(AGitIDEClient: TGitIDEClient);
  end;

  TParentUpdateGitMenu = class(TGitMenu)
  protected
    function GetImageIndex: Integer; override;
  public
    constructor Create;
  end;

  TRootDirUpdateGitMenu = class(TBaseUpdateGitMenu)
  public
    constructor Create(AGitIDEClient: TGitIDEClient);
  end;

  TProjectDirUpdateGitMenu = class(TBaseUpdateGitMenu)
  public
    constructor Create(AGitIDEClient: TGitIDEClient);
  end;

  TExpicitFilesUpdateGitMenu = class(TBaseUpdateGitMenu)
  public
    constructor Create(AGitIDEClient: TGitIDEClient);
  end;

  TFileUpdateGitMenu = class(TBaseUpdateGitMenu)
  protected
    function GetImageIndex: Integer; override;
  public
    constructor Create(AGitIDEClient: TGitIDEClient);
  end;

  TDirUpdateGitMenu = class(TBaseUpdateGitMenu)
  protected
    function GetImageIndex: Integer; override;
  public
    constructor Create(AGitIDEClient: TGitIDEClient);
  end;

  procedure DoPull(const GitClient: TGitClient; const ProjectDir: string);

implementation

uses Forms, Controls, Windows, GitIDEConst, SysUtils, GitIDEMessageView,
  ActiveX, IStreams, GitClientUpdate, Generics.Defaults, Generics.Collections,
  GitUIUtils, GitIDEUtils, Graphics, GitIDEIcons, Dialogs;

const
  sPMVUpdate = 'Update';
  sPMVUpdateParent = 'UpdateParent';
  sPMVUpdateRootDir = 'UpdateRootDir';
  sPMVUpdateProjectDir = 'UpdateProjectDir';
  sPMVUpdateExpicitFiles = 'UpdateExpicitFiles';
  sPMVUpdateDir = 'UpdateDir';

var
  Aborted: Boolean;


{ TBaseUpdateSvnMenu }

constructor TBaseUpdateGitMenu.Create(AGitIDEClient: TGitIDEClient);
begin
  inherited;
  FCaption := sPMMUpdate;
  FVerb := sPMVUpdate;
  FHelpContext := 0;
  FGitIDEClient := AGitIDEClient;
  FRootType := rtRootDir;
end;

procedure TBaseUpdateGitMenu.Execute(const MenuContextList: IInterfaceList);
var
  Module: IOTAModule;
  I: Integer;
  Project: IOTAProject;
  ProjectDir: string;
begin
  SaveAll;
  Aborted := False;
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
    DoPull(FGitIDEClient.GitClient, ProjectDir);
  end
  else
    MessageDlg(sGitClientNotConfigured, mtError, [mbOk,mbHelp], hcGitClone);
end;

procedure DoPull(const GitClient: TGitClient; const ProjectDir: string);
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
    SvnMessageView.WriteTitle(sUpdating + ' project directory ' + ProjectDir);
    GitClient.Pull(ProjectDir, Output);
    begin
      OutputStrings.Text := Output;
      for I := 0 to OutputStrings.Count - 1 do
        SvnMessageView.WriteTitle(OutputStrings[I]);
    end;
  finally
    Screen.Cursor := Cursor;
    OutputStrings.Free;
  end;
end;


{ TParentUpdateSvnMenu }

constructor TParentUpdateGitMenu.Create;
begin
  inherited Create(nil);
  FCaption := sPMMUpdate;
  FVerb := sPMVUpdateParent;
  FParent := sPMVGitParent;
  FPosition := pmmpParentUpdateSvnMenu;
  FHelpContext := 0;
end;

function TParentUpdateGitMenu.GetImageIndex: Integer;
begin
  Result := UpdateImageIndex;
end;

{ TRootDirUpdateSvnMenu }

constructor TRootDirUpdateGitMenu.Create(AGitIDEClient: TGitIDEClient);
begin
  inherited Create(AGitIDEClient);
  FRootType := rtRootDir;
  FParent := sPMVUpdateParent;
  FCaption := sPMMRootDir;
  FVerb := sPMVUpdateRootDir;
  FPosition := pmmpRootDirUpdateSvnMenu;
  FHelpContext := 0;
end;

{ TProjectDirUpdateSvnMenu }

constructor TProjectDirUpdateGitMenu.Create(AGitIDEClient: TGitIDEClient);
begin
  inherited Create(AGitIDEClient);
  FRootType := rtProjectDir;
  FParent := sPMVUpdateParent;
  FCaption := sPMMProjectDir;
  FVerb := sPMVUpdateProjectDir;
  FPosition := pmmpProjectDirUpdateSvnMenu;
  FHelpContext := 0;
end;

{ TExpicitFilesUpdateSvnMenu }

constructor TExpicitFilesUpdateGitMenu.Create(AGitIDEClient: TGitIDEClient);
begin
  inherited Create(AGitIDEClient);
  FRootType := rtExpicitFiles;
  FParent := sPMVUpdateParent;
  FCaption := sPMMExpicitFiles;
  FVerb := sPMVUpdateExpicitFiles;
  FPosition := pmmpExpicitFilesUpdateSvnMenu;
  FHelpContext := 0;
end;

{ TFileUpdateSvnMenu }

constructor TFileUpdateGitMenu.Create(AGitIDEClient: TGitIDEClient);
begin
  inherited Create(AGitIDEClient);
  FRootType := rtRootDir;
  FParent := sPMVGitParent;
  FCaption := sPMMUpdate;
  FVerb := sPMVUpdate;
  FPosition := pmmpFileUpdateSvnMenu;
  FHelpContext := 0;
end;

function TFileUpdateGitMenu.GetImageIndex: Integer;
begin
  Result := UpdateImageIndex;
end;

{ TDirUpdateSvnMenu }

constructor TDirUpdateGitMenu.Create(AGitIDEClient: TGitIDEClient);
begin
  inherited Create(AGitIDEClient);
  FRootType := rtDir;
  FParent := sPMVGitParent;
  FCaption := sPMMUpdate;
  FVerb := sPMVUpdateDir;
  FPosition := pmmpFileUpdateSvnMenu;
  FHelpContext := 0;
end;

function TDirUpdateGitMenu.GetImageIndex: Integer;
begin
  Result := UpdateImageIndex;
end;


end.
