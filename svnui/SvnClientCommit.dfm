object SvnCommitForm: TSvnCommitForm
  Left = 0
  Top = 0
  Caption = 'Commit'
  ClientHeight = 494
  ClientWidth = 589
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  DesignSize = (
    589
    494)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 52
    Height = 13
    Caption = 'Commit to:'
  end
  object Location: TLabel
    Left = 66
    Top = 8
    Width = 40
    Height = 13
    Caption = 'Location'
  end
  object Label2: TLabel
    Left = 8
    Top = 308
    Width = 45
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Comment'
    ExplicitTop = 309
  end
  object Comment: TMemo
    Left = 8
    Top = 327
    Width = 573
    Height = 129
    Anchors = [akLeft, akRight, akBottom]
    TabOrder = 0
    OnKeyPress = CommentKeyPress
    ExplicitTop = 328
    ExplicitWidth = 570
  end
  object Recent: TButton
    Left = 8
    Top = 462
    Width = 98
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Recent Comments'
    TabOrder = 1
    ExplicitTop = 463
  end
  object Help: TButton
    Left = 506
    Top = 462
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Help'
    TabOrder = 2
    ExplicitLeft = 503
    ExplicitTop = 463
  end
  object Cancel: TButton
    Left = 425
    Top = 462
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
    ExplicitLeft = 422
    ExplicitTop = 463
  end
  object OK: TButton
    Left = 344
    Top = 462
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Enabled = False
    ModalResult = 1
    TabOrder = 4
    OnClick = OKClick
    ExplicitLeft = 341
    ExplicitTop = 463
  end
  object Files: TListView
    Left = 8
    Top = 27
    Width = 573
    Height = 275
    Anchors = [akLeft, akTop, akRight, akBottom]
    Checkboxes = True
    Columns = <
      item
        AutoSize = True
        Caption = 'Name'
      end
      item
        AutoSize = True
        Caption = 'Path'
      end
      item
        AutoSize = True
        Caption = 'Ext'
      end
      item
        AutoSize = True
        Caption = 'Status'
      end>
    MultiSelect = True
    SortType = stBoth
    TabOrder = 5
    ViewStyle = vsReport
    OnColumnClick = FilesColumnClick
    OnCompare = FilesCompare
    ExplicitWidth = 570
    ExplicitHeight = 276
  end
end
