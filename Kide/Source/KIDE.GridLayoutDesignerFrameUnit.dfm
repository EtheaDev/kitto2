inherited GridLayoutDesignerFrame: TGridLayoutDesignerFrame
  Width = 741
  Height = 419
  HelpKeyword = 'GridPanel#Grid_layouts'
  ExplicitWidth = 741
  ExplicitHeight = 419
  inherited ClientPanel: TPanel
    Width = 720
    Height = 419
    ExplicitWidth = 720
    ExplicitHeight = 419
    inherited DesignPanel: TPanel
      Width = 720
      Height = 362
      ExplicitWidth = 720
      ExplicitHeight = 362
      inherited ToolBar: TToolBar
        Width = 720
        ExplicitWidth = 720
        object ToolButton1: TToolButton
          Left = 46
          Top = 0
          Width = 8
          Caption = 'ToolButtonGrid1'
          ImageIndex = 29
          Style = tbsSeparator
        end
        object ToolButton2: TToolButton
          Left = 54
          Top = 0
          Action = UpdateLayoutAction
        end
      end
      object EditorPageControl: TPageControl
        Left = 0
        Top = 22
        Width = 720
        Height = 340
        ActivePage = MainTabSheet
        Align = alClient
        TabOrder = 1
        object MainTabSheet: TTabSheet
          Caption = 'Grid Layout'
          ImageIndex = -1
          object DBGrid: TDBGrid
            Left = 0
            Top = 0
            Width = 712
            Height = 112
            Cursor = crHandPoint
            Align = alClient
            DataSource = DataSource
            Options = [dgTitles, dgIndicator, dgColumnResize, dgColLines, dgRowLines, dgTabs, dgConfirmDelete, dgCancelOnExit, dgTitleClick, dgTitleHotTrack]
            TabOrder = 0
            TitleFont.Charset = DEFAULT_CHARSET
            TitleFont.Color = clWindowText
            TitleFont.Height = -11
            TitleFont.Name = 'Tahoma'
            TitleFont.Style = []
            OnCellClick = DBGridCellClick
            OnColEnter = DBGridColEnter
            OnColExit = DBGridColExit
            OnColumnMoved = DBGridColumnMoved
            OnDrawColumnCell = DBGridDrawColumnCell
            OnDblClick = DBGridDblClick
            OnEnter = DBGridEnter
          end
          object FieldPanel: TPanel
            Left = 0
            Top = 112
            Width = 712
            Height = 200
            Align = alBottom
            TabOrder = 1
            DesignSize = (
              712
              200)
            object DisplayWidthLabel: TLabel
              Left = 127
              Top = 84
              Width = 65
              Height = 13
              Caption = 'Display Width'
            end
            object AlignmentLabel: TLabel
              Left = 204
              Top = 84
              Width = 47
              Height = 13
              Caption = 'Alignment'
            end
            object DisplayLabelEdit: TLabeledEdit
              Left = 6
              Top = 57
              Width = 701
              Height = 21
              Anchors = [akLeft, akTop, akRight]
              EditLabel.Width = 62
              EditLabel.Height = 13
              EditLabel.Caption = 'Display Label'
              TabOrder = 1
              OnChange = DisplayLabelEditChange
            end
            object DisplayWidthEdit: TSpinEdit
              Left = 125
              Top = 101
              Width = 73
              Height = 22
              MaxValue = 1000
              MinValue = 1
              TabOrder = 3
              Value = 1
              OnChange = DisplayWidthEditChange
            end
            object DisplayFormatEdit: TLabeledEdit
              Left = 6
              Top = 146
              Width = 701
              Height = 21
              Anchors = [akLeft, akTop, akRight]
              EditLabel.Width = 71
              EditLabel.Height = 13
              EditLabel.Caption = 'Display Format'
              TabOrder = 5
              OnChange = DisplayFormatEditChange
            end
            object AlignmentComboBox: TComboBox
              Left = 204
              Top = 101
              Width = 503
              Height = 21
              Style = csDropDownList
              Anchors = [akLeft, akTop, akRight]
              TabOrder = 4
              OnChange = AlignmentComboBoxChange
              Items.Strings = (
                'Left'
                'Right'
                'Center')
            end
            object FieldNameEdit: TLabeledEdit
              Left = 5
              Top = 18
              Width = 702
              Height = 21
              Anchors = [akLeft, akTop, akRight]
              EditLabel.Width = 49
              EditLabel.Height = 13
              EditLabel.Caption = 'FieldName'
              ReadOnly = True
              TabOrder = 0
            end
            object IsReadOnlyCheckBox: TCheckBox
              Left = 6
              Top = 101
              Width = 113
              Height = 17
              Caption = 'IsReadOnly'
              TabOrder = 2
              OnClick = IsReadOnlyCheckBoxClick
            end
          end
        end
      end
    end
    inherited PathPanel: TStaticText
      Width = 720
    end
    inherited CodeEditorFrame: TCodeEditorFrame
      Top = 379
      Width = 720
      ExplicitTop = 379
      ExplicitWidth = 720
      inherited ErrorLabel: TLabel
        Width = 720
        ExplicitTop = 342
        ExplicitWidth = 416
      end
      inherited ToolBar: TToolBar
        Width = 720
        ExplicitWidth = 720
      end
    end
  end
  inherited TabSet: TTabSet
    Left = 720
    Height = 419
    ExplicitLeft = 720
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
  object ClientDataSet: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 440
    Top = 104
  end
  object DataSource: TDataSource
    DataSet = ClientDataSet
    Left = 320
    Top = 128
  end
end
