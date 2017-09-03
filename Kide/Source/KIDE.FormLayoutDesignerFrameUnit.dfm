inherited FormLayoutDesignerFrame: TFormLayoutDesignerFrame
  Width = 689
  Height = 419
  HelpKeyword = 'Form#Layouts'
  ExplicitWidth = 689
  ExplicitHeight = 419
  inherited ClientPanel: TPanel
    Width = 668
    Height = 419
    ExplicitWidth = 668
    ExplicitHeight = 419
    inherited DesignPanel: TPanel
      Width = 668
      Height = 362
      ExplicitWidth = 668
      ExplicitHeight = 362
      inherited ToolBar: TToolBar
        Width = 668
        ExplicitWidth = 668
        object ToolButton1: TToolButton
          Left = 100
          Top = 0
          Width = 8
          Caption = 'ToolButtonSep'
          ImageIndex = 29
          Style = tbsSeparator
        end
        object UpdateLayoutToolButton: TToolButton
          Left = 108
          Top = 0
          Action = UpdateLayoutAction
        end
      end
      object EditorPageControl: TPageControl
        Left = 0
        Top = 22
        Width = 668
        Height = 340
        ActivePage = OptionsTabSheet
        Align = alClient
        TabOrder = 1
        object OptionsTabSheet: TTabSheet
          Caption = 'Options'
          ImageIndex = -1
          DesignSize = (
            660
            312)
          object MemoWidthLabel: TLabel
            Left = 3
            Top = 4
            Width = 56
            Height = 13
            Caption = 'MemoWidth'
          end
          object MaxFieldWidthLabel: TLabel
            Left = 82
            Top = 4
            Width = 70
            Height = 13
            Caption = 'MaxFieldWidth'
          end
          object MinFieldWidthLabel: TLabel
            Left = 161
            Top = 4
            Width = 66
            Height = 13
            Caption = 'MinFieldWidth'
          end
          object MsgTargetLabel: TLabel
            Left = 3
            Top = 86
            Width = 51
            Height = 13
            Caption = 'MsgTarget'
          end
          object LabelWidthLabel: TLabel
            Left = 82
            Top = 45
            Width = 53
            Height = 13
            Caption = 'LabelWidth'
          end
          object LabelAlignLabel: TLabel
            Left = 3
            Top = 45
            Width = 48
            Height = 13
            Caption = 'LabelAlign'
          end
          object _MemoWidth: TSpinEdit
            Left = 3
            Top = 20
            Width = 73
            Height = 22
            Increment = 10
            MaxValue = 0
            MinValue = 0
            TabOrder = 0
            Value = 0
          end
          object _MaxFieldWidth: TSpinEdit
            Left = 82
            Top = 20
            Width = 73
            Height = 22
            MaxValue = 0
            MinValue = 0
            TabOrder = 1
            Value = 0
          end
          object _MinFieldWidth: TSpinEdit
            Left = 161
            Top = 20
            Width = 73
            Height = 22
            MaxValue = 0
            MinValue = 0
            TabOrder = 2
            Value = 0
          end
          object MsgTargetComboBox: TComboBox
            Left = 3
            Top = 103
            Width = 152
            Height = 21
            Style = csDropDownList
            TabOrder = 6
            Items.Strings = (
              'Qtip'
              'Title'
              'Under'
              'Side')
          end
          object _RequiredLabelTemplate: TLabeledEdit
            Left = 240
            Top = 60
            Width = 417
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            EditLabel.Width = 112
            EditLabel.Height = 13
            EditLabel.Caption = 'RequiredLabelTemplate'
            TabOrder = 7
          end
          object _LabelWidth: TSpinEdit
            Left = 82
            Top = 60
            Width = 73
            Height = 22
            MaxValue = 0
            MinValue = 0
            TabOrder = 4
            Value = 0
          end
          object LabelAlignComboBox: TComboBox
            Left = 3
            Top = 60
            Width = 73
            Height = 21
            Style = csDropDownList
            TabOrder = 3
            Items.Strings = (
              'Right'
              'Left'
              'Top')
          end
          object _LabelSeparator: TLabeledEdit
            Left = 161
            Top = 60
            Width = 73
            Height = 21
            EditLabel.Width = 73
            EditLabel.Height = 13
            EditLabel.Caption = 'LabelSeparator'
            TabOrder = 5
          end
        end
      end
    end
    inherited PathPanel: TStaticText
      Width = 668
      ExplicitWidth = 668
    end
    inherited CodeEditorFrame: TCodeEditorFrame
      Top = 379
      Width = 668
      ExplicitTop = 379
      ExplicitWidth = 668
      inherited ErrorLabel: TLabel
        Width = 668
        ExplicitTop = 342
        ExplicitWidth = 416
      end
      inherited ToolBar: TToolBar
        Width = 668
        ExplicitWidth = 668
      end
    end
  end
  inherited TabSet: TTabSet
    Left = 668
    Height = 419
    ExplicitLeft = 668
    ExplicitHeight = 419
  end
  inherited ActionList: TActionList
    Left = 288
    Top = 24
    object UpdateLayoutAction: TAction
      Caption = 'Update fields'
      Hint = 'Update layout fields from View fields'
      ImageIndex = 8
      OnExecute = UpdateLayoutActionExecute
    end
  end
end
