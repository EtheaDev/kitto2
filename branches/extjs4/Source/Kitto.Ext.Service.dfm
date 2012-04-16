object KExtService: TKExtService
  OldCreateOrder = False
  DisplayName = 'KittoService'
  OnShutdown = ServiceShutdown
  OnStart = ServiceStart
  OnStop = ServiceStop
  Height = 150
  Width = 215
end
