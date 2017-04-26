unit Cayan.POSServer;

(*
  Cayan POS Web Server Service


*)

interface

uses
  System.Classes, System.SysUtils,
  IdContext, IdCustomHTTPServer, IdBaseComponent, IdComponent,
  IdCustomTCPServer, IdHTTPServer, IdTCPConnection, IdYarn,
  Cayan.Common, Cayan.Genius.Intf, Cayan.MWv4.Intf;

type
  TCayanPOSServer = class;
  TCayanPOSServerThread = class;

  TCayanPOSServerStatus = (cssOffline, cssOnline, cssBusy, cssError, cssNeedsUpdate);

  TCayanPOSServer = class(TComponent)
  private
    FStatus: TCayanPOSServerStatus;
    FThread: TCayanPOSServerThread;
    procedure SetActive(const Value: Boolean);
    procedure Start;
    procedure Stop;
    function GetActive: Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Active: Boolean read GetActive write SetActive;
  end;

  TCayanPOSServerThread = class(TThread)
  private
    FOwner: TCayanPOSServer;
    procedure Init;
    procedure Uninit;
    procedure Process;
  protected
    procedure Execute; override;
  public
    constructor Create(AOwner: TCayanPOSServer); reintroduce;
    destructor Destroy; override;
  end;

implementation

{ TCayanPOSServer }

constructor TCayanPOSServer.Create(AOwner: TComponent);
begin
  inherited;

end;

destructor TCayanPOSServer.Destroy;
begin

  inherited;
end;

function TCayanPOSServer.GetActive: Boolean;
begin
  Result:= FStatus <> TCayanPOSServerStatus.cssOffline;
end;

procedure TCayanPOSServer.SetActive(const Value: Boolean);
begin
  if Value then begin
    if not Active then begin
      Start;
    end;
  end else begin
    if Active then begin
      Stop;
    end;
  end;
end;

procedure TCayanPOSServer.Start;
begin
  FStatus:= TCayanPOSServerStatus.cssBusy;
  try
    FThread:= TCayanPOSServerThread.Create(Self);
    //TODO: Set event handlers...

    FThread.Start;

    FStatus:= TCayanPOSServerStatus.cssOnline; //TODO: Move to after binding...

  except
    on E: Exception do begin
      FStatus:= TCayanPOSServerStatus.cssError;
      //TODO: Log Exception and Trigger Event
    end;
  end;
end;

procedure TCayanPOSServer.Stop;
var
  T: TCayanPOSServerThread;
begin
  FStatus:= TCayanPOSServerStatus.cssOffline;
  if Assigned(FThread) then begin
    //Acquire a pointer so we make FThread nil before terminating.
    //At this point, we don't care what happens to the thread
    //after we terminate it.
    T:= FThread;
    FThread:= nil;
    T.FreeOnTerminate:= True;
    T.Terminate;
  end;
end;

{ TCayanPOSServerThread }

constructor TCayanPOSServerThread.Create(AOwner: TCayanPOSServer);
begin
  inherited Create(True);
  FOwner:= AOwner;
end;

destructor TCayanPOSServerThread.Destroy;
begin

  inherited;
end;

procedure TCayanPOSServerThread.Execute;
begin
  Init;
  try
    while not Terminated do begin
      try
        Process;
      except
        on E: Exception do begin
          //TODO: Handle exception...

        end;
      end;
    end;
  finally
    Uninit;
  end;
end;

procedure TCayanPOSServerThread.Process;
begin
  //TODO: Save log, process requests, etc...

end;

procedure TCayanPOSServerThread.Init;
begin

end;

procedure TCayanPOSServerThread.Uninit;
begin

end;

end.
