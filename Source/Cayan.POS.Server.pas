unit Cayan.POS.Server;

(*
  Cayan POS Web Server Service


*)

interface

uses
  System.Classes, System.SysUtils, System.SyncObjs,
  IdContext, IdCustomHTTPServer, IdBaseComponent, IdComponent,
  IdCustomTCPServer, IdHTTPServer, IdTCPConnection, IdYarn,
  DB, ADODB, Winapi.ActiveX,
  Cayan.Common,
  Cayan.Genius.Intf,
  Cayan.MWv4.Intf,
  Cayan.XSuperObject,
  XmlIntf, XmlDoc, XmlDom;

type
  TCayanPOSServer = class;
  TCayanPOSServerThread = class;
  TCayanPOSServerContext = class;

  TCayanPOSServerStatus = (cssOffline, cssStarting, cssOnline, cssBusy, cssError, cssNeedsUpdate);

  TCayanPOSServerVer = (csv1, csv2);

  TCayanPOSServerStatusEvent = procedure(Sender: TObject; const Status: TCayanPOSServerStatus) of object;

  TCayanPOSServer = class(TComponent)
  private
    FStatus: TCayanPOSServerStatus;
    FThread: TCayanPOSServerThread;
    FOnStatus: TCayanPOSServerStatusEvent;
    FPort: Integer;
    FConnStr: String;
    procedure SetActive(const Value: Boolean);
    function GetActive: Boolean;
    procedure ChangeStatus(const Status: TCayanPOSServerStatus);
    procedure ThreadStatus(Sender: TObject; const Status: TCayanPOSServerStatus);
    procedure SetPort(const Value: Integer);
    procedure SetConnStr(const Value: String);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Start;
    procedure Stop;
  published
    property Active: Boolean read GetActive write SetActive;
    property Port: Integer read FPort write SetPort;
    property ConnStr: String read FConnStr write SetConnStr;

    property OnStatus: TCayanPOSServerStatusEvent read FOnStatus write FOnStatus;
  end;

  TCayanPOSServerThread = class(TThread)
  private
    FOwner: TCayanPOSServer;
    FServer: TIdHTTPServer;
    FStatus: TCayanPOSServerStatus;
    FOnStatus: TCayanPOSServerStatusEvent;
    FPort: Integer;
    FConnStr: String;
    FLog: TStringList;
    FLogLock: TCriticalSection;
    procedure Init;
    procedure Uninit;
    procedure Process;
    procedure ChangeStatus(const Status: TCayanPOSServerStatus);
    procedure ThreadAfterBind(Sender: TObject);
    procedure ThreadCommand(AContext: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
    procedure ThreadConnect(AContext: TIdContext);
    procedure ThreadDisconnect(AContext: TIdContext);
    procedure SetConnStr(const Value: String);
    procedure SetPort(const Value: Integer);
    procedure PostLog(const S: String);
  protected
    procedure Execute; override;
    procedure SYNC_OnStatus;
  public
    constructor Create(AOwner: TCayanPOSServer); reintroduce;
    destructor Destroy; override;
    property Port: Integer read FPort write SetPort;
    property ConnStr: String read FConnStr write SetConnStr;

    property OnStatus: TCayanPOSServerStatusEvent read FOnStatus write FOnStatus;
  end;

  TCayanPOSServerContext = class(TIdServerContext)
  private
    FOwner: TCayanPOSServerThread;
    FDoc: TStringList;
    FPar: TStringList;
    FVer: TCayanPOSServerVer;
    FResponse: TCombinedData;
    FFormat: TDataFormat;
    FDB: TADOConnection;
    FQry: TADOQuery;
  public
    constructor Create(AConnection: TIdTCPConnection; AYarn: TIdYarn;
      AList: TIdContextThreadList = nil); override;
    destructor Destroy; override;
    procedure Init(AOwner: TCayanPOSServerThread);
    procedure Uninit;
    function NewQuery: TADOQuery;
    procedure HandleRequest(const AReq: TIdHTTPRequestInfo; ARes: TIdHTTPResponseInfo);
    procedure HandleGetStatus(const AReq: TIdHTTPRequestInfo; ARes: TIdHTTPResponseInfo);
    procedure HandleGetLog(const AReq: TIdHTTPRequestInfo; ARes: TIdHTTPResponseInfo);
    procedure HandleGetCustomers(const AReq: TIdHTTPRequestInfo; ARes: TIdHTTPResponseInfo);
    procedure HandlePostUserLogin(const AReq: TIdHTTPRequestInfo; ARes: TIdHTTPResponseInfo);
  end;

implementation

uses
  StrUtils;

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
  if csDesigning in ComponentState then begin

  end else begin
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
end;

procedure TCayanPOSServer.SetConnStr(const Value: String);
begin
  if Self.Active then
    raise Exception.Create('Cannot set port while active.');
  FConnStr := Value;
end;

procedure TCayanPOSServer.SetPort(const Value: Integer);
begin
  if Self.Active then
    raise Exception.Create('Cannot set port while active.');
  FPort := Value;
end;

procedure TCayanPOSServer.ChangeStatus(const Status: TCayanPOSServerStatus);
begin
  FStatus:= Status;
  if Assigned(Self.FOnStatus) then
    FOnStatus(Self, Status);
end;

procedure TCayanPOSServer.Start;
begin
  FStatus:= TCayanPOSServerStatus.cssBusy;
  try
    ChangeStatus(cssStarting);
    FThread:= TCayanPOSServerThread.Create(Self);
    //TODO: Set event handlers...
    FThread.Port:= Port;
    FThread.ConnStr:= ConnStr;
    FThread.OnStatus:= ThreadStatus;
    FThread.Start;
  except
    on E: Exception do begin
      ChangeStatus(cssError);
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
    //T.FreeOnTerminate:= True;
    T.Terminate;

    T.WaitFor;
  end;
end;

procedure TCayanPOSServer.ThreadStatus(Sender: TObject;
  const Status: TCayanPOSServerStatus);
begin
  Self.ChangeStatus(Status);
end;

{ TCayanPOSServerThread }

constructor TCayanPOSServerThread.Create(AOwner: TCayanPOSServer);
begin
  inherited Create(True);
  FOwner:= AOwner;
  FLog:= TStringList.Create;
  FLogLock:= TCriticalSection.Create;
end;

destructor TCayanPOSServerThread.Destroy;
begin
  FLogLock.Enter;
  try
    FLog.Clear;
  finally
    FLogLock.Leave;
  end;
  FreeAndNil(FLogLock);
  FreeAndNil(FLog);
  inherited;
end;

procedure TCayanPOSServerThread.Init;
begin
  PostLog('TCayanPOSServerThread.Init');
  CoInitialize(nil);
  FServer:= TIdHTTPServer.Create(nil);
  FServer.ContextClass:= TCayanPOSServerContext;
  FServer.OnAfterBind:= ThreadAfterBind;
  FServer.OnConnect:= ThreadConnect;
  FServer.OnCommandGet:= ThreadCommand;
  FServer.OnCommandOther:= ThreadCommand;
  FServer.DefaultPort:= FPort;
  FServer.Active:= True;
end;

procedure TCayanPOSServerThread.Uninit;
begin
  PostLog('TCayanPOSServerThread.Uninit');
  try
    Self.ChangeStatus(TCayanPOSServerStatus.cssOffline);
  finally
    FServer.Active:= False;
  end;
  FServer.Contexts.ClearAndFree; //TODO: ???

  FreeAndNil(FServer);
  CoUninitialize;
end;

procedure TCayanPOSServerThread.Execute;
begin
  Init;
  try
    while not Terminated do begin
      try
        try
          Process;
        finally
          Sleep(1);
        end;
      except
        on E: Exception do begin
          //TODO: Handle exception...
          PostLog('TCayanPOSServerThread.Execute EXCEPTION: ' + E.Message);
        end;
      end;
      Sleep(1);
    end;
  finally
    Uninit;
  end;
end;

procedure TCayanPOSServerThread.PostLog(const S: String);
begin
  FLogLock.Enter;
  try
    FLog.Append(FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', Now) + ' - ' + S);
  finally
    FLogLock.Leave;
  end;
end;

procedure TCayanPOSServerThread.Process;
begin
  //TODO: Save log, process requests, etc...

end;

procedure TCayanPOSServerThread.ChangeStatus(
  const Status: TCayanPOSServerStatus);
begin
  FStatus:= Status;
  case Status of
    cssOffline:     PostLog('Status Changed: Offline');
    cssStarting:    PostLog('Status Changed: Starting');
    cssOnline:      PostLog('Status Changed: Online');
    cssBusy:        PostLog('Status Changed: Busy');
    cssError:       PostLog('Status Changed: Error');
    cssNeedsUpdate: PostLog('Status Changed: Needs Update');
  end;
  Synchronize(SYNC_OnStatus);
end;

procedure TCayanPOSServerThread.SetConnStr(const Value: String);
begin
  FConnStr := Value;
end;

procedure TCayanPOSServerThread.SetPort(const Value: Integer);
begin
  FPort := Value;
end;

procedure TCayanPOSServerThread.SYNC_OnStatus;
begin
  if Assigned(FOnStatus) then
    FOnStatus(Self, FStatus);
end;

procedure TCayanPOSServerThread.ThreadAfterBind(Sender: TObject);
begin
  ChangeStatus(TCayanPOSServerStatus.cssOnline);
end;

procedure TCayanPOSServerThread.ThreadConnect(AContext: TIdContext);
var
  C: TCayanPOSServerContext;
begin
  C:= TCayanPOSServerContext(AContext);
  C.Init(Self);
  PostLog('Client Connected: ' + C.Connection.Socket.Binding.PeerIP);
end;

procedure TCayanPOSServerThread.ThreadDisconnect(AContext: TIdContext);
var
  C: TCayanPOSServerContext;
begin
  C:= TCayanPOSServerContext(AContext);
  PostLog('Client Disconnected: ' + C.Connection.Socket.Binding.PeerIP);
  C.Uninit;
end;

procedure TCayanPOSServerThread.ThreadCommand(AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
var
  C: TCayanPOSServerContext;
begin
  C:= TCayanPOSServerContext(AContext);
  C.HandleRequest(ARequestInfo, AResponseInfo);
end;

{ TCayanPOSServerContext }

constructor TCayanPOSServerContext.Create(AConnection: TIdTCPConnection;
  AYarn: TIdYarn; AList: TIdContextThreadList);
begin
  inherited;
  FDoc:= TStringList.Create;
  FPar:= TStringList.Create;
  FResponse:= TCombinedData.Create;
end;

destructor TCayanPOSServerContext.Destroy;
begin
  FreeAndNil(FResponse);
  FreeAndNil(FPar);
  FreeAndNil(FDoc);
  inherited;
end;

procedure TCayanPOSServerContext.HandleRequest(const AReq: TIdHTTPRequestInfo;
  ARes: TIdHTTPResponseInfo);
var
  U: String;
  P: Integer;
  Str: TMemoryStream;
  F: String;
  function IsAct(const S: String): Boolean;
  begin
    Result:= SameText(S, FDoc[1]);
  end;
begin
  FOwner.PostLog(AReq.Command + ' ' + AReq.URI);
  FPar.Assign(AReq.Params);

  F:= FPar.Values['Format'];
  if F = '' then
    Self.FFormat:= efJSON
  else if F = 'XML' then
    Self.FFormat:= efXML
  else if F = 'JSON' then
    Self.FFormat:= efJSON
  else begin
    ARes.ResponseNo:= 400;
  end;

  FResponse.Start(FFormat);
  try
    FDoc.Clear;
    U:= AReq.Document;
    Delete(U, 1, 1);
    U:= U + '/';
    while Length(U) > 0 do begin
      P:= Pos('/', U);
      FDoc.Append(Copy(U, 1, P-1));
      Delete(U, 1, P);
    end;

    if FDoc.Count = 0 then begin
      FResponse.AddStr('error', 'No API version was supplised.');
    end else begin
      if FDoc.Count = 1 then begin
        FResponse.AddStr('error', 'No API action was supplised.');
      end else begin

        if FDoc[0] = 'v1' then begin
          FVer:= TCayanPOSServerVer.csv1;
        end else
        if FDoc[0] = 'v2' then begin
          FVer:= TCayanPOSServerVer.csv2;
        end else begin
          ARes.ResponseNo:= 400;
        end;

        case AReq.CommandType of
          hcGET: begin
            if IsAct('Status') then begin
              HandleGetStatus(AReq, ARes);
            end else if IsAct('Log') then begin
              HandleGetLog(AReq, ARes);
            end else if IsAct('Customers') then begin
              HandleGetCustomers(AReq, ARes);
            end else begin
              //TODO
            end;
          end;
          hcPOST: begin
            AReq.PostStream.Position:= 0;
            if IsAct('UserLogin') then begin
              HandlePostUserLogin(AReq, ARes);
            end else begin
              //TODO
            end;
          end;
          hcDELETE: begin

          end;
          hcPUT: begin

          end;
          else begin

          end;
        end;

      end;
    end;
  finally
    case FFormat of
      efXML:  ARes.ContentType:= 'text/xml';
      efJSON: ARes.ContentType:= 'text/json';
    end;
    ARes.ContentText:= FResponse.AsString;
  end;
end;

procedure TCayanPOSServerContext.Init(AOwner: TCayanPOSServerThread);
begin
  FOwner:= AOwner;
  CoInitialize(nil);
  FDB:= TADOConnection.Create(nil);
  FDB.LoginPrompt:= False;
  FDB.ConnectionString:= AOwner.ConnStr;
  FQry:= NewQuery;
  try
    FDB.Connected:= True;
  except
    on E: Exception do begin
      FOwner.PostLog('ERROR: Failed to connect to database: ' + E.Message);
    end;
  end;
end;

procedure TCayanPOSServerContext.Uninit;
begin
  FDB.Connected:= False;
  FreeAndNil(FQry);
  FreeAndNil(FDB);
  CoUninitialize;
end;

function TCayanPOSServerContext.NewQuery: TADOQuery;
begin
  Result:= TADOQuery.Create(nil);
  Result.Connection:= FDB;
end;

procedure TCayanPOSServerContext.HandleGetStatus(const AReq: TIdHTTPRequestInfo; ARes: TIdHTTPResponseInfo);
begin
  FResponse.RootElement:= 'Status';
  FResponse.AddStr('status', 'active');
  FResponse.AddStr('timestamp', FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', Now));
  FResponse.AddStr('db_conn', IfThen(FDB.Connected, 'true', 'false'));
end;

procedure TCayanPOSServerContext.HandleGetCustomers(
  const AReq: TIdHTTPRequestInfo; ARes: TIdHTTPResponseInfo);
var
  Q: String;
  A: ISuperArray;
  CO: ISuperObject;
  D: IXmlDocument;
  N, CN: IXmlNode;
  procedure AddStr(const N: String; const V: String);
  var
    T: IXmlNode;
  begin
    case FFormat of
      efXML: begin
        T:= CN.AddChild(N); T.Text:= V;
      end;
      efJSON: CO.S[N]:= V;
    end;
  end;
  procedure AddInt(const N: String; const V: Integer);
  begin
    case FFormat of
      efXML:  AddStr(N, IntToStr(V));
      efJSON: CO.I[N]:= V;
    end;
  end;
begin
  Q:= FPar.Values['q'];

  FQry.Close;
  FQry.SQL.Clear;
  FQry.Parameters.Clear;
  FQry.SQL.Text:= 'select * from Customers';
  if Q <> '' then begin
    FQry.SQL.Append('where FirstName like :fn or LastName like :ln or CompanyName like :cn');
    FQry.Parameters.ParamValues['fn']:= Q;
    FQry.Parameters.ParamValues['ln']:= Q;
    FQry.Parameters.ParamValues['cn']:= Q;
  end;

  FQry.Open;
  try
    case FFormat of
      efXML: begin
        D:= NewXmlDocument;
        N:= D.AddChild('Customers');
        while not FQry.Eof do begin
          CN:= N.AddChild('Customer');
          AddInt('ID', FQry.FieldByName('ID').AsInteger);
          AddStr('FirstName', FQry.FieldByName('FirstName').AsString);
          AddStr('LastName', FQry.FieldByName('LastName').AsString);
          AddStr('CompanyName', FQry.FieldByName('CompanyName').AsString);

          FQry.Next;
        end;

        FResponse.LoadXML(D.XML.Text);
      end;
      efJSON: begin
        A:= SA;
        while not FQry.Eof do begin
          try
            CO:= SO;
            AddInt('ID', FQry.FieldByName('ID').AsInteger);
            AddStr('FirstName', FQry.FieldByName('FirstName').AsString);
            AddStr('LastName', FQry.FieldByName('LastName').AsString);
            AddStr('CompanyName', FQry.FieldByName('CompanyName').AsString);

            FQry.Next;
          finally
            A.Add(CO);
          end;
        end;

        FResponse.LoadJSON(A.AsJson(True));
      end;
    end;
  finally
    FQry.Close;
  end;
end;

procedure TCayanPOSServerContext.HandleGetLog(const AReq: TIdHTTPRequestInfo; ARes: TIdHTTPResponseInfo);
var
  X: Integer;
begin
  FResponse.RootElement:= 'Log';
  FOwner.FLogLock.Enter;
  try
    for X := 0 to FOwner.FLog.Count-1 do begin
      FResponse.AddStr(IntToStr(X), FOwner.FLog[X]);
    end;
  finally
    FOwner.FLogLock.Leave;
  end;
end;

procedure TCayanPOSServerContext.HandlePostUserLogin(const AReq: TIdHTTPRequestInfo; ARes: TIdHTTPResponseInfo);
var
  UN, PW: String;
  Q: TADOQuery;
begin
  Self.FResponse.RootElement:= 'LoginResult';
  UN:= FPar.Values['user'];
  PW:= FPar.Values['pass'];

  FResponse.AddStr('user', UN);

  FQry.Close;
  FQry.SQL.Clear;
  FQry.Parameters.Clear;
  FQry.SQL.Text:= 'select * from Users where Username = :un and StatusID = 1';
  FQry.Parameters.ParamValues['un']:= UN;
  FQry.Open;
  try
    if not FQry.IsEmpty then begin
      //User exists, validate password...
      if PW = FQry.FieldByName('Password').AsString then begin
        //Valid password...
        Q:= Self.NewQuery;
        try
          Q.SQL.Text:= 'select * from UserSessions where 1<>1';
          Q.Open;
          try
            Q.Append;
            try
              Q['UserID']:= FQry.FieldByName('ID').AsInteger;
              Q['Timestamp']:= Now;
              Q['Ping']:= Now;
              Q['StatusID']:= 1;
            finally
              Q.Post;
            end;
            FResponse.AddStr('Status', 'Success');
            FResponse.AddStr('Token', Q.FieldByName('ID').AsString);
            FResponse.AddInt('UserID', FQry.FieldByName('ID').AsInteger);
          finally
            Q.Close;
          end;
        finally
          Q.Free;
        end;
      end else begin
        //Invalid password
        FResponse.AddStr('Status', 'Failed');
      end;
    end else begin
      //User does not exist
      FResponse.AddStr('Status', 'Failed');
    end;
  finally
    FQry.Close;
  end;
end;

end.
