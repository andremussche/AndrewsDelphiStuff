unit leap.Core;

{$R 'leapmotionts-1.0.9+8391.sms.js'}

interface

const
   LEAPMOTION_INIT: string = "leapMotionInit";
   LEAPMOTION_CONNECTED: string = "leapMotionConnected";
   LEAPMOTION_DISCONNECTED: string = "leapMotionDisconnected";
   LEAPMOTION_EXIT: string = "leapMotionExit";
   LEAPMOTION_FRAME: string = "leapMotionFrame";

{forward type declarations}
type
  Jboolean= boolean;
  JWebSocket= class(JObject);
  //TType= class(JObject);
  //JState= class(JObject);

  JLeapEvent = class;
  JController = class;
  JInteractionBox = class;
  JPointable = class;
  JGesture = class;
  JFinger = class;
  JTool = class;
  JHand = class;
  JFrame = class;
  JMatrix = class;
  JVector3 = class;

  JFunction= procedure;
  JLeapEventFunction = procedure(event:JLeapEvent);

TZone = (ZONE_NONE = 0, ZONE_HOVERING = 1, ZONE_TOUCHING = 2);
TState = (STATE_INVALID = 0, STATE_START = 1, STATE_UPDATE = 2, STATE_STOP = 3);
TType = (TYPE_INVALID = 4, TYPE_SWIPE = 5, TYPE_CIRCLE = 6, TYPE_SCREEN_TAP = 7, TYPE_KEY_TAP = 8);

  JEventDispatcher = class external "EventDispatcher"
  public
    procedure constructor_();
    function hasEventListener(&type: string; listener: JLeapEventFunction): Jboolean;
    procedure addEventListener(typeStr: string; listenerFunction: JLeapEventFunction);
    procedure removeEventListener(typeStr: string; listenerFunction: JLeapEventFunction);
    procedure dispatchEvent(event: JLeapEvent);
    property {private} listeners: variant;
  end;
  JListener = class external "Object"
  public
    procedure onConnect(controller: JController);
    procedure onDisconnect(controller: JController);
    procedure onExit(controller: JController);
    procedure onFrame(controller: JController; frame: JFrame);
    procedure onInit(controller: JController);
  end;
  JDefaultListener = class external "DefaultListener" (JListener)
  public
    procedure constructor_();
    procedure onConnect(controller: JController);
    procedure onDisconnect(controller: JController);
    procedure onExit(controller: JController);
    procedure onFrame(controller: JController; frame: JFrame);
    procedure onInit(controller: JController);
  end;
  JLeapEvent = class external "Leap.LeapEvent"
  public
    procedure constructor_(&type: string; targetListener: JListener; frame: JFrame);overload;
    function getTarget(): variant;
    function getType(): string;
//    class var {static?} LEAPMOTION_INIT: string = "leapMotionInit";
//    class property {static?} LEAPMOTION_CONNECTED: string = "leapMotionConnected";
//    class property {static?} LEAPMOTION_DISCONNECTED: string = "leapMotionDisconnected";
//    class property {static?} LEAPMOTION_EXIT: string = "leapMotionExit";
//    class property {static?} LEAPMOTION_FRAME: string = "leapMotionFrame";
    property {private} _type: variant;
    property {private} _target: variant;
    property {public} frame: JFrame;
  end;
  JLeapUtil = class external "Leap.LeapUtil"
  public
    procedure constructor_();
    function toDegrees(radians: integer): integer;
    function isNearZero(value: integer): Jboolean;
    function vectorIsNearZero(inVector: JVector3): Jboolean;
    function extractRotation(mtxTransform: JMatrix): JMatrix;
    function rotationInverse(mtxRot: JMatrix): JMatrix;
    function rigidInverse(mtxTransform: JMatrix): JMatrix;
    function componentWiseMin(vLHS: JVector3; vRHS: JVector3): JVector3;
    function componentWiseMax(vLHS: JVector3; vRHS: JVector3): JVector3;
    function componentWiseScale(vLHS: JVector3; vRHS: JVector3): JVector3;
    function componentWiseReciprocal(inVector: JVector3): JVector3;
    function minComponent(inVector: JVector3): integer;
    function maxComponent(inVector: JVector3): integer;
    function heading(inVector: JVector3): integer;
    function elevation(inVector: JVector3): integer;
    function normalizeSpherical(vSpherical: JVector3): JVector3;
    function cartesianToSpherical(vCartesian: JVector3): JVector3;
    function sphericalToCartesian(vSpherical: JVector3): JVector3;
    function clamp(inVal: integer; minVal: integer; maxVal: integer): integer;
    function lerp(a: integer; b: integer; coefficient: integer): integer;
    function lerpVector(vec1: JVector3; vec2: JVector3; coefficient: integer): JVector3;
    class property {static?} PI: integer;
    class property {static?} DEG_TO_RAD: integer;
    class property {static?} RAD_TO_DEG: integer;
    class property {static?} TWO_PI: integer;
    class property {static?} HALF_PI: integer;
    class property {static?} EPSILON: integer;
  end;
  JController = class external "Leap.Controller" (JEventDispatcher)
  public
    procedure constructor_(host: string);overload;
    procedure getHandByID(frame: Variant; id: Variant);
    procedure getPointableByID(frame: Variant; id: Variant);
    function frame(history: integer): JFrame;overload;
    procedure setListener(listener: JListener);
    procedure enableGesture(&type: TType; enable: Jboolean);overload;
    function isGestureEnabled(&type: TType): Jboolean;
    function isConnected(): Jboolean;
    property {private} listener: variant;
    property {public} frameHistory: array of JFrame;
    property {private} latestFrame: variant;
    property {public} connection: JWebSocket;
    property {public} _isConnected: Jboolean;
    property {public} _isGesturesEnabled: Jboolean;
  end;
  JInteractionBox = class external "Leap.InteractionBox"
  public
    procedure constructor_();
    function denormalizePoint(normalizedPosition: JVector3): JVector3;
    function normalizePoint(position: JVector3; clamp: Jboolean): JVector3;overload;
    function isValid(): Jboolean;
    function isEqualTo(other: JInteractionBox): Jboolean;
    function invalid(): JInteractionBox;
    function toString(): string;
    property {public} center: JVector3;
    property {public} depth: integer;
    property {public} height: integer;
    property {public} width: integer;
  end;

  JPointable = class external "Leap.Pointable"
  public
    procedure constructor_();
    function isValid(): Jboolean;
    function isEqualTo(other: JPointable): Jboolean;
    function invalid(): JPointable;
    function toString(): string;
    property {public} touchZone: integer;
    property {public} touchDistance: integer;
    property {public} direction: JVector3;
    property {public} frame: JFrame;
    property {public} hand: JHand;
    property {public} id: integer;
    property {public} length: integer;
    property {public} width: integer;
    property {public} tipPosition: JVector3;
    property {public} stabilizedTipPosition: JVector3;
    property {public} timeVisible: integer;
    property {public} tipVelocity: JVector3;
    property {public} isFinger: Jboolean;
    property {public} isTool: Jboolean;
  end;

  JGesture = class external "Leap.Gesture"
  public
    procedure constructor_();
    function isEqualTo(other: JGesture): Jboolean;
    function isValid(): Jboolean;
    function invalid(): JGesture;
    function toString(): string;
    property {public} duration: integer;
    property {public} durationSeconds: integer;
    property {public} frame: JFrame;
    property {public} hands: array of JHand;
    property {public} id: integer;
    property {public} pointables: array of JPointable;
    property {public} state: TState;
    property {public} &type: TType;
  end;
  JFinger = class external "Leap.Finger" (JPointable)
  public
    procedure constructor_();
    function invalid(): JFinger;
  end;
  JTool = class external "Leap.Tool" (JPointable)
  public
    procedure constructor_();
    function invalid(): JTool;
  end;
  JHand = class external "Leap.Hand"
  public
    procedure constructor_();
    function isValid(): Jboolean;
    function isEqualTo(other: JHand): Jboolean;
    function finger(id: integer): JFinger;
    function tool(id: integer): JTool;
    function pointable(id: integer): JPointable;
    function rotationAxis(sinceFrame: JFrame): JVector3;
    function rotationAngle(sinceFrame: JFrame; axis: JVector3): integer;overload;
    function rotationMatrix(sinceFrame: JFrame): JMatrix;
    function scaleFactor(sinceFrame: JFrame): integer;
    function translation(sinceFrame: JFrame): JVector3;
    function invalid(): JHand;
    property {public} direction: JVector3;
    property {public} fingers: array of JFinger;
    property {public} frame: JFrame;
    property {public} id: integer;
    property {public} palmNormal: JVector3;
    property {public} palmPosition: JVector3;
    property {public} stabilizedPalmPosition: JVector3;
    property {public} timeVisible: integer;
    property {public} palmVelocity: JVector3;
    property {public} pointables: array of JPointable;
    property {public} sphereCenter: JVector3;
    property {public} sphereRadius: integer;
    property {public} tools: array of JTool;
    property {public} rotation: JMatrix;
    property {public} scaleFactorNumber: integer;
    property {public} translationVector: JVector3;
  end;
  JFrame = class external "Leap.Frame"
  public
    procedure constructor_();
    function hand(id: integer): JHand;
    function finger(id: integer): JFinger;
    function tool(id: integer): JTool;
    function pointable(id: integer): JPointable;
    function gesture(id: integer): JGesture;
    function gestures(sinceFrame: JFrame): array of JGesture;overload;
    function rotationAxis(sinceFrame: JFrame): JVector3;
    function rotationAngle(sinceFrame: JFrame; axis: JVector3): integer;overload;
    function rotationMatrix(sinceFrame: JFrame): JMatrix;
    function scaleFactor(sinceFrame: JFrame): integer;
    function translation(sinceFrame: JFrame): JVector3;
    function isEqualTo(other: JFrame): Jboolean;
    function isValid(): Jboolean;
    function invalid(): JFrame;
    property {public} fingers: array of JFinger;
    property {public} hands: array of JHand;
    property {public} pointables: array of JPointable;
    property {public} _gestures: array of JGesture;
    property {public} id: integer;
    property {public} currentFramesPerSecond: integer;
    property {public} interactionBox: JInteractionBox;
    property {public} timestamp: integer;
    property {public} tools: array of JTool;
    property {public} rotation: JMatrix;
    property {public} scaleFactorNumber: integer;
    property {public} translationVector: JVector3;
    property {public} controller: JController;
  end;
  JMatrix = class external "Leap.Matrix"
  public
    procedure constructor_(x: JVector3; y: JVector3; z: JVector3; _origin: JVector3);overload;
    procedure setRotation(_axis: JVector3; angleRadians: integer);
    function transformPoint(inVector: JVector3): JVector3;
    function transformDirection(inVector: JVector3): JVector3;
    function rigidInverse(): JMatrix;
    function multiply(other: JMatrix): JMatrix;
    function multiplyAssign(other: JMatrix): JMatrix;
    function isEqualTo(other: JMatrix): Jboolean;
    function identity(): JMatrix;
    function toString(): string;
    property {public} origin: JVector3;
    property {public} xBasis: JVector3;
    property {public} yBasis: JVector3;
    property {public} zBasis: JVector3;
  end;
  JCircleGesture = class external "Leap.CircleGesture" (JGesture)
  public
    procedure constructor_();
    class property {static?} classType: integer;
    property {public} center: JVector3;
    property {public} normal: JVector3;
    property {public} pointable: JPointable;
    property {public} progress: integer;
    property {public} radius: integer;
  end;
  JKeyTapGesture = class external "Leap.KeyTapGesture" (JGesture)
  public
    procedure constructor_();
    class property {static?} classType: integer;
    property {public} direction: JVector3;
    property {public} pointable: JPointable;
    property {public} position: JVector3;
    property {public} progress: integer;
  end;
  JScreenTapGesture = class external "Leap.ScreenTapGesture" (JGesture)
  public
    procedure constructor_();
    class property {static?} classType: integer;
    property {public} direction: JVector3;
    property {public} pointable: JPointable;
    property {public} position: JVector3;
    property {public} progress: integer;
  end;
  JSwipeGesture = class external "Leap.SwipeGesture" (JGesture)
  public
    procedure constructor_();
    class property {static?} classType: integer;
    property {public} direction: JVector3;
    property {public} pointable: JPointable;
    property {public} position: JVector3;
    property {public} speed: integer;
    property {public} startPosition: JVector3;
  end;
  JVector3 = class external "Leap.Vector3"
  public
    procedure constructor_(x: integer; y: integer; z: integer);
    function opposite(): JVector3;
    function plus(other: JVector3): JVector3;
    function plusAssign(other: JVector3): JVector3;
    function minus(other: JVector3): JVector3;
    function minusAssign(other: JVector3): JVector3;
    function multiply(scalar: integer): JVector3;
    function multiplyAssign(scalar: integer): JVector3;
    function divide(scalar: integer): JVector3;
    function divideAssign(scalar: integer): JVector3;
    function isEqualTo(other: JVector3): Jboolean;
    function angleTo(other: JVector3): integer;
    function cross(other: JVector3): JVector3;
    function distanceTo(other: JVector3): integer;
    function dot(other: JVector3): integer;
    function isValid(): Jboolean;
    function invalid(): JVector3;
    function magnitude(): integer;
    function magnitudeSquared(): integer;
    function normalized(): JVector3;
    function zero(): JVector3;
    function xAxis(): JVector3;
    function yAxis(): JVector3;
    function zAxis(): JVector3;
    function left(): JVector3;
    function right(): JVector3;
    function down(): JVector3;
    function up(): JVector3;
    function forward(): JVector3;
    function backward(): JVector3;
    function toString(): string;
    property {public} x: integer;
    property {public} y: integer;
    property {public} z: integer;
    property {public} pitch: integer;
    property {public} yaw: integer;
    property {public} roll: integer;
  end;


implementation

end.

