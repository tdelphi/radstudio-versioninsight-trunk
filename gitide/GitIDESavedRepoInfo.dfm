object frmSavedRepoInfo: TfrmSavedRepoInfo
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Remove saved authentication data:'
  ClientHeight = 347
  ClientWidth = 429
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object ListView1: TListView
    Left = 0
    Top = 0
    Width = 429
    Height = 296
    Align = alClient
    Checkboxes = True
    Columns = <
      item
        AutoSize = True
        Caption = 'Ident'
      end
      item
        AutoSize = True
        Caption = 'RemoteUrl'
      end>
    TabOrder = 0
    ViewStyle = vsReport
  end
  object Panel1: TPanel
    Left = 0
    Top = 296
    Width = 429
    Height = 51
    Align = alBottom
    TabOrder = 1
    object BtnOk: TButton
      Left = 256
      Top = 16
      Width = 75
      Height = 25
      Caption = 'Ok'
      ModalResult = 1
      TabOrder = 0
    end
    object BtnCancel: TButton
      Left = 345
      Top = 16
      Width = 75
      Height = 25
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
  end
end
