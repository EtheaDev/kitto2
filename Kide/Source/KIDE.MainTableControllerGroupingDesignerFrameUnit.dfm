inherited MainTableControllerGroupingDesignerFrame: TMainTableControllerGroupingDesignerFrame
  Width = 497
  Height = 185
  HelpKeyword = 'GridPanel#grouping'
  ExplicitWidth = 497
  ExplicitHeight = 185
  inherited ClientPanel: TPanel
    Width = 497
    Height = 185
    ExplicitWidth = 497
    ExplicitHeight = 185
    inherited DesignPanel: TPanel
      Width = 497
      Height = 185
      ExplicitWidth = 497
      ExplicitHeight = 185
      object _FieldName: TLabeledEdit
        Left = 7
        Top = 22
        Width = 236
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        EditLabel.Width = 49
        EditLabel.Height = 13
        EditLabel.Caption = 'FieldName'
        TabOrder = 0
      end
      object _SortFieldNames: TLabeledEdit
        Left = 249
        Top = 22
        Width = 240
        Height = 21
        Anchors = [akTop, akRight]
        EditLabel.Width = 74
        EditLabel.Height = 13
        EditLabel.Caption = 'SortFieldNames'
        TabOrder = 1
      end
      object _EnableMenu: TCheckBox
        Left = 7
        Top = 51
        Width = 90
        Height = 17
        Caption = 'EnableMenu'
        TabOrder = 2
      end
      object _StartCollapsed: TCheckBox
        Left = 96
        Top = 51
        Width = 90
        Height = 17
        Caption = 'StartCollapsed'
        TabOrder = 3
      end
      object _ShowName: TCheckBox
        Left = 197
        Top = 51
        Width = 90
        Height = 17
        Caption = 'ShowName'
        TabOrder = 4
      end
      object ShowCountGroupBox: TGroupBox
        Left = 6
        Top = 77
        Width = 483
        Height = 92
        Anchors = [akLeft, akTop, akRight]
        Enabled = False
        TabOrder = 5
        DesignSize = (
          483
          92)
        object _ShowCount_Template: TLabeledEdit
          Left = 7
          Top = 26
          Width = 468
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          EditLabel.Width = 44
          EditLabel.Height = 13
          EditLabel.Caption = 'Template'
          TabOrder = 0
        end
        object _ShowCount_PluralItemName: TLabeledEdit
          Left = 7
          Top = 64
          Width = 256
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          EditLabel.Width = 75
          EditLabel.Height = 13
          EditLabel.Caption = 'PluralItemName'
          TabOrder = 1
        end
        object _ShowCount_ItemName: TLabeledEdit
          Left = 269
          Top = 64
          Width = 206
          Height = 21
          Anchors = [akTop, akRight]
          EditLabel.Width = 49
          EditLabel.Height = 13
          EditLabel.Caption = 'ItemName'
          TabOrder = 2
        end
      end
      object _ShowCount: TCheckBox
        Left = 11
        Top = 70
        Width = 79
        Height = 17
        Caption = 'ShowCount'
        TabOrder = 6
        OnClick = _ShowCountClick
      end
    end
  end
end
