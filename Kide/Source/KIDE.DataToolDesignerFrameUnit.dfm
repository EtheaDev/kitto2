inherited DataToolDesignerFrame: TDataToolDesignerFrame
  inherited ClientPanel: TPanel
    inherited DesignPanel: TPanel
      inherited ControllerGroupBox: TGroupBox
        object DataToolGroupBox: TGroupBox
          Left = 2
          Top = 75
          Width = 448
          Height = 60
          Align = alTop
          Caption = 'DataTool'
          TabOrder = 1
          DesignSize = (
            448
            60)
          object AutoRefreshLabel: TLabel
            Left = 6
            Top = 16
            Width = 61
            Height = 13
            Alignment = taRightJustify
            Caption = 'AutoRefresh'
          end
          object _AutoRefresh: TComboBox
            Left = 6
            Top = 32
            Width = 316
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            TabOrder = 0
            Items.Strings = (
              'Current'
              'All')
          end
          object _RequireSelection: TCheckBox
            Left = 328
            Top = 13
            Width = 97
            Height = 17
            Anchors = [akTop, akRight]
            Caption = 'RequireSelection'
            TabOrder = 1
          end
          object _LoadAllRecords: TCheckBox
            Left = 328
            Top = 33
            Width = 97
            Height = 17
            Anchors = [akTop, akRight]
            Caption = 'LoadAllRecords'
            TabOrder = 2
          end
        end
      end
    end
  end
end
