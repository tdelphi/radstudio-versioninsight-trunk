object SvnConflictDialog: TSvnConflictDialog
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Merge Conflict'
  ClientHeight = 192
  ClientWidth = 538
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  DesignSize = (
    538
    192)
  PixelsPerInch = 96
  TextHeight = 13
  object Description: TLabel
    Left = 8
    Top = 8
    Width = 509
    Height = 26
    Anchors = [akLeft, akTop, akRight]
    Caption = 
      'An error occurred when updating file %s.  The file contains loca' +
      'l changes that subversion can not resolve with changes made to t' +
      'he same file on the server.  Resolve this conflict by:'
    WordWrap = True
    ExplicitWidth = 508
  end
  object Local: TRadioButton
    Left = 8
    Top = 56
    Width = 522
    Height = 18
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Using your local file, ignoring changes from the server'
    TabOrder = 0
  end
  object Server: TRadioButton
    Left = 8
    Top = 80
    Width = 522
    Height = 18
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Using the file from the server, overwriting your local changes'
    TabOrder = 1
  end
  object Merge: TRadioButton
    Left = 8
    Top = 104
    Width = 522
    Height = 18
    Anchors = [akLeft, akTop, akRight]
    Caption = 
      'Merging where possible, leaving the remaining conflicts in the f' +
      'ile'
    Checked = True
    TabOrder = 2
    TabStop = True
  end
  object Ok: TButton
    Left = 224
    Top = 159
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 4
  end
  object MergeView: TRadioButton
    Left = 8
    Top = 128
    Width = 522
    Height = 17
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Using %s merge viewer'
    TabOrder = 3
  end
end
