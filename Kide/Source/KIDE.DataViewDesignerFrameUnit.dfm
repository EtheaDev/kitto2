inherited DataViewDesignerFrame: TDataViewDesignerFrame
  HelpKeyword = 'DataView'
  inherited ClientPanel: TPanel
    inherited DesignPanel: TPanel
      inherited VCPageControl: TPageControl
        inherited ViewTabSheet: TTabSheet
          ExplicitLeft = 4
          ExplicitTop = 24
          ExplicitWidth = 489
          ExplicitHeight = 290
          inherited ViewGroupBox: TGroupBox
            Height = 145
            ExplicitHeight = 145
            object _IsLookup: TCheckBox [7]
              Left = 129
              Top = 113
              Width = 97
              Height = 17
              Caption = 'IsLookup'
              TabOrder = 5
            end
            inherited ImageNameEdit: TLabeledEdit
              TabOrder = 4
            end
            inherited ControllerComboBox: TComboBox
              Items.Strings = (
                'List'
                'Form')
            end
            object _Type: TLabeledEdit
              Left = 6
              Top = 113
              Width = 114
              Height = 21
              EditLabel.Width = 24
              EditLabel.Height = 13
              EditLabel.Caption = 'Type'
              ReadOnly = True
              TabOrder = 0
            end
          end
        end
        inherited ControllerTabSheet: TTabSheet
          ExplicitLeft = 4
          ExplicitTop = 24
          ExplicitWidth = 489
          ExplicitHeight = 290
        end
        object MainTableTabSheet: TTabSheet
          Caption = 'MainTable'
          ImageIndex = 2
        end
      end
    end
  end
end
