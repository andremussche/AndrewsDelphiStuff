unit three.THREE;

interface

uses
  three.Core;

type
  JColor = class;
  JFace = class;
  JFace3 = class;
  JFace4 = class;
  JFrustum = class;
  JLine3 = class;
  JPlane = class;
  JSphere = class;
  JBoundingBox3D = class;
  JBoundingSphere = class;
  JGeometry = class;
  JMatrix = class;
  JMatrix3 = class;
  JMatrix4 = class;
  JRay = class;
  JObject3D = class;
  JQuaternion = class;
  JRaycaster = class;
  JTriangle = class;
  JVector = class;
  JVector2 = class;
  JVector3 = class;
  JVector4 = class;
  JBox2 = class;
  JBox3 = class;
  JCamera = class;
  JLight = class;
  JDirectionalLight = class;
  JJSONLoader = class;
  JMaterial = class;
  JLineBasicMaterial = class;
  JLineDashedMaterial = class;
  JMeshBasicMaterial = class;
  JMeshDepthMaterial = class;
  JMeshFaceMaterial = class;
  JMeshLambertMaterial = class;
  JMeshNormalMaterial = class;
  JMeshPhongMaterial = class;
  JParticleBasicMaterial = class;
  JParticleCanvasMaterial = class;
  JParticleDOMMaterial = class;
  JShaderMaterial = class;
  JLine = class;
  JLOD = class;
  JMesh = class;
  JMorphAnimMesh = class;
  JParticle = class;
  JParticleSystem = class;
  JRibbon = class;
  JSkinnedMesh = class;
  JSpriteMaterial = class;
  JSprite = class;
  JWebGLRenderer = class;
  JRenderTarget = class;
  JWebGLRenderTarget = class;
  JRenderableVertex = class;
  JIFog = class;
  JFog = class;
  JFogExp2 = class;
  JScene = class;
  JTexture = class;
  JCompressedTexture = class;
  JDataTexture = class;
  JAnimationData = class;
  JAnimationInterpolation = class;
  JPath = class;
  JShape = class;
  JExtrudeGeometry = class;
  JPolyhedronGeometry = class;
  JShapeGeometry = class;
  JLensFlare = class;

TCullFace = variant;
TFrontFaceDirection = variant;
TShadowMapType = variant;
TSide = variant;
TShading = variant;
TColors = variant;
TBlending = variant;
TBlendingEquation = variant;
TBlendingDstFactor = variant;
TBlendingSrcFactor = variant;
TCombine = variant;
TMapping = variant;
TWrapping = variant;
TTextureFilter = variant;
TTextureDataType = variant;
TPixelType = variant;
TPixelFormat = variant;
TCompressedPixelFormat = variant;

  JMappingConstructor = class external "Object"
  public
    function &new(): JMapping;
  end;
  JArrayBufferView = class external "Object"
  end;
  JBufferGeometryAttributeArray = class external(JArrayBufferView)
    property length: integer;
  end;
  JBufferGeometryAttribute = class external "Object"
    property itemSize: integer;
    property &array: JBufferGeometryAttributeArray;
    property numItems: integer;
  end;
  JBufferGeometryAttributes = class external "Object"
  public
    function  GetItems(name: string): JBufferGeometryAttribute; external array;
    procedure SetItems(name: string; value: JBufferGeometryAttribute); external array;
    property Items[name: string]: JBufferGeometryAttribute read GetItems write SetItems; default;
    property index: JBufferGeometryAttribute;
    property position: JBufferGeometryAttribute;
    property normal: JBufferGeometryAttribute;
    property color: JBufferGeometryAttribute;
  end;

  TBufferGeometry_offsets_object = class;

  JBufferGeometry = class external "Object"
  public
    constructor Create();
    procedure applyMatrix(matrix: JMatrix4);
    procedure computeBoundingBox();
    procedure computeBoundingSphere();
    procedure computeVertexNormals();
    procedure normalizeNormals();
    procedure computeTangents();
    procedure dispose();
    property id: integer;
    property attributes: JBufferGeometryAttributes;
    property dynamic: Jboolean;
    property boundingBox: JBoundingBox3D;
    property boundingSphere: JBoundingSphere;
    property hasTangents: Jboolean;
    property morphTargets: array of variant;
    property offsets: array of TBufferGeometry_offsets_object;
    property verticesNeedUpdate: Jboolean;
  end;
  TBufferGeometry_offsets_object = class external "Object"
    property start: integer;
    property count: integer;
    property index: integer;
  end;
  JClock = class external "Object"
  public
    constructor Create(autoStart: Jboolean);overload;
    procedure start();
    procedure stop();
    function getElapsedTime(): integer;
    function getDelta(): integer;
    property autoStart: Jboolean;
    property startTime: integer;
    property oldTime: integer;
    property elapsedTime: integer;
    property running: Jboolean;
  end;
  JHSL = class external "Object"
    property h: integer;
    property s: integer;
    property l: integer;
  end;
  JColor = class external "Object"
  public
    constructor Create(hex: string);overload;
    constructor Create(hex: integer);overload;
    procedure &set(value: integer);overload;
    procedure &set(value: string);overload;
    function setHex(hex: integer): JColor;
    function setRGB(r: integer; g: integer; b: integer): JColor;
    function setHSL(h: integer; s: integer; l: integer): JColor;
    function setStyle(style: string): JColor;
    function copy(color: JColor): JColor;
    function copyGammaToLinear(color: JColor): JColor;
    function copyLinearToGamma(color: JColor): JColor;
    function convertGammaToLinear(): JColor;
    function convertLinearToGamma(): JColor;
    function getHex(): integer;
    function getHexString(): string;
    function getHSL(): JHSL;
    function getStyle(): string;
    function offsetHSL(h: integer; s: integer; l: integer): JColor;
    function add(color: JColor): JColor;
    function addColors(color1: JColor; color2: JColor): JColor;
    function addScalar(s: integer): JColor;
    function multiply(color: JColor): JColor;
    function multiplyScalar(s: integer): JColor;
    function lerp(color: JColor; alpha: integer): JColor;
    function clone(): JColor;
    property r: integer;
    property g: integer;
    property b: integer;
  end;
  TColorKeywords = class external "ColorKeywords"
  public
    function  GetItems(name: string): integer; external array;
    procedure SetItems(name: string; value: integer); external array;
    property Items[name: string]: integer read GetItems write SetItems; default;
    property aliceblue: integer;
    property antiquewhite: integer;
    property aqua: integer;
    property aquamarine: integer;
    property azure: integer;
    property beige: integer;
    property bisque: integer;
    property black: integer;
    property blanchedalmond: integer;
    property blue: integer;
    property blueviolet: integer;
    property brown: integer;
    property burlywood: integer;
    property cadetblue: integer;
    property chartreuse: integer;
    property chocolate: integer;
    property coral: integer;
    property cornflowerblue: integer;
    property cornsilk: integer;
    property crimson: integer;
    property cyan: integer;
    property darkblue: integer;
    property darkcyan: integer;
    property darkgoldenrod: integer;
    property darkgray: integer;
    property darkgreen: integer;
    property darkgrey: integer;
    property darkkhaki: integer;
    property darkmagenta: integer;
    property darkolivegreen: integer;
    property darkorange: integer;
    property darkorchid: integer;
    property darkred: integer;
    property darksalmon: integer;
    property darkseagreen: integer;
    property darkslateblue: integer;
    property darkslategray: integer;
    property darkslategrey: integer;
    property darkturquoise: integer;
    property darkviolet: integer;
    property deeppink: integer;
    property deepskyblue: integer;
    property dimgray: integer;
    property dimgrey: integer;
    property dodgerblue: integer;
    property firebrick: integer;
    property floralwhite: integer;
    property forestgreen: integer;
    property fuchsia: integer;
    property gainsboro: integer;
    property ghostwhite: integer;
    property gold: integer;
    property goldenrod: integer;
    property gray: integer;
    property green: integer;
    property greenyellow: integer;
    property grey: integer;
    property honeydew: integer;
    property hotpink: integer;
    property indianred: integer;
    property indigo: integer;
    property ivory: integer;
    property khaki: integer;
    property lavender: integer;
    property lavenderblush: integer;
    property lawngreen: integer;
    property lemonchiffon: integer;
    property lightblue: integer;
    property lightcoral: integer;
    property lightcyan: integer;
    property lightgoldenrodyellow: integer;
    property lightgray: integer;
    property lightgreen: integer;
    property lightgrey: integer;
    property lightpink: integer;
    property lightsalmon: integer;
    property lightseagreen: integer;
    property lightskyblue: integer;
    property lightslategray: integer;
    property lightslategrey: integer;
    property lightsteelblue: integer;
    property lightyellow: integer;
    property lime: integer;
    property limegreen: integer;
    property linen: integer;
    property magenta: integer;
    property maroon: integer;
    property mediumaquamarine: integer;
    property mediumblue: integer;
    property mediumorchid: integer;
    property mediumpurple: integer;
    property mediumseagreen: integer;
    property mediumslateblue: integer;
    property mediumspringgreen: integer;
    property mediumturquoise: integer;
    property mediumvioletred: integer;
    property midnightblue: integer;
    property mintcream: integer;
    property mistyrose: integer;
    property moccasin: integer;
    property navajowhite: integer;
    property navy: integer;
    property oldlace: integer;
    property olive: integer;
    property olivedrab: integer;
    property orange: integer;
    property orangered: integer;
    property orchid: integer;
    property palegoldenrod: integer;
    property palegreen: integer;
    property paleturquoise: integer;
    property palevioletred: integer;
    property papayawhip: integer;
    property peachpuff: integer;
    property peru: integer;
    property pink: integer;
    property plum: integer;
    property powderblue: integer;
    property purple: integer;
    property red: integer;
    property rosybrown: integer;
    property royalblue: integer;
    property saddlebrown: integer;
    property salmon: integer;
    property sandybrown: integer;
    property seagreen: integer;
    property seashell: integer;
    property sienna: integer;
    property silver: integer;
    property skyblue: integer;
    property slateblue: integer;
    property slategray: integer;
    property slategrey: integer;
    property snow: integer;
    property springgreen: integer;
    property steelblue: integer;
    property tan: integer;
    property teal: integer;
    property thistle: integer;
    property tomato: integer;
    property turquoise: integer;
    property violet: integer;
    property wheat: integer;
    property white: integer;
    property whitesmoke: integer;
    property yellow: integer;
    property yellowgreen: integer;
  end;

  TEventDispatcher_addEventListener_listener_ = procedure (event: variant);
  TEventDispatcher_dispatchEvent_event_object = class;
  TEventDispatcher_removeEventListener_listener_ = procedure (event: variant);

  JEventDispatcher = class external "Object"
  public
    constructor Create();
    procedure addEventListener(&type: string; listener: TEventDispatcher_addEventListener_listener_);
    procedure dispatchEvent(event: TEventDispatcher_dispatchEvent_event_object);
    procedure removeEventListener(&type: string; listener: TEventDispatcher_removeEventListener_listener_);
  end;
  TEventDispatcher_dispatchEvent_event_object = class external "Object"
    property &type: string;
    property target: variant;
  end;
  JFace = class external "Object"
  public
    function clone(): JFace;
    property normal: JVector3;
    property color: JColor;
    property vertexNormals: array of JVector3;
    property vertexColors: array of JColor;
    property vertexTangents: array of integer;
    property materialIndex: integer;
    property centroid: JVector3;
  end;
  JFace3 = class external(JFace)
  public
    constructor Create(a: integer; b: integer; c: integer; normal: JVector3);overload;
    constructor Create(a: integer; b: integer; c: integer; normal: JVector3;color: JColor);overload;
    constructor Create(a: integer; b: integer; c: integer; normal: JVector3;color: JColor;materialIndex: integer);overload;
    constructor Create(a: integer; b: integer; c: integer; normal: JVector3;vertexColors: array of JColor);overload;
    constructor Create(a: integer; b: integer; c: integer; normal: JVector3;vertexColors: array of JColor;materialIndex: integer);overload;
    constructor Create(a: integer; b: integer; c: integer; vertexNormals: array of JVector3);overload;
    constructor Create(a: integer; b: integer; c: integer; vertexNormals: array of JVector3;color: JColor);overload;
    constructor Create(a: integer; b: integer; c: integer; vertexNormals: array of JVector3;color: JColor;materialIndex: integer);overload;
    constructor Create(a: integer; b: integer; c: integer; vertexNormals: array of JVector3;vertexColors: array of JColor);overload;
    constructor Create(a: integer; b: integer; c: integer; vertexNormals: array of JVector3;vertexColors: array of JColor;materialIndex: integer);overload;
    function clone(): JFace3;
    property a: integer;
    property b: integer;
    property c: integer;
    property normal: JVector3;
    property color: JColor;
    property vertexNormals: array of JVector3;
    property vertexColors: array of JColor;
    property vertexTangents: array of integer;
    property materialIndex: integer;
    property centroid: JVector3;
  end;
  JFace4 = class external(JFace)
  public
    constructor Create(a: integer; b: integer; c: integer; d: integer; normal: JVector3);overload;
    constructor Create(a: integer; b: integer; c: integer; d: integer; normal: JVector3;color: JColor);overload;
    constructor Create(a: integer; b: integer; c: integer; d: integer; normal: JVector3;color: JColor;materialIndex: integer);overload;
    constructor Create(a: integer; b: integer; c: integer; d: integer; normal: JVector3;vertexColors: array of JColor);overload;
    constructor Create(a: integer; b: integer; c: integer; d: integer; normal: JVector3;vertexColors: array of JColor;materialIndex: integer);overload;
    constructor Create(a: integer; b: integer; c: integer; d: integer; vertexNormals: array of JVector3);overload;
    constructor Create(a: integer; b: integer; c: integer; d: integer; vertexNormals: array of JVector3;color: JColor);overload;
    constructor Create(a: integer; b: integer; c: integer; d: integer; vertexNormals: array of JVector3;color: JColor;materialIndex: integer);overload;
    constructor Create(a: integer; b: integer; c: integer; d: integer; vertexNormals: array of JVector3;vertexColors: array of JColor);overload;
    constructor Create(a: integer; b: integer; c: integer; d: integer; vertexNormals: array of JVector3;vertexColors: array of JColor;materialIndex: integer);overload;
    function clone(): JFace4;
    property a: integer;
    property b: integer;
    property c: integer;
    property d: integer;
    property normal: JVector3;
    property color: JColor;
    property vertexNormals: array of JVector3;
    property vertexColors: array of JColor;
    property vertexTangents: array of integer;
    property materialIndex: integer;
    property centroid: JVector3;
  end;
  JFrustum = class external "Object"
  public
    constructor Create(p0: JPlane);overload;
    constructor Create(p0: JPlane;p1: JPlane);overload;
    constructor Create(p0: JPlane;p1: JPlane;p2: JPlane);overload;
    constructor Create(p0: JPlane;p1: JPlane;p2: JPlane;p3: JPlane);overload;
    constructor Create(p0: JPlane;p1: JPlane;p2: JPlane;p3: JPlane;p4: JPlane);overload;
    constructor Create(p0: JPlane;p1: JPlane;p2: JPlane;p3: JPlane;p4: JPlane;p5: JPlane);overload;
    function &set(p0: integer): JFrustum;overload;
    function &set(p0: integer;p1: integer): JFrustum;overload;
    function &set(p0: integer;p1: integer;p2: integer): JFrustum;overload;
    function &set(p0: integer;p1: integer;p2: integer;p3: integer): JFrustum;overload;
    function &set(p0: integer;p1: integer;p2: integer;p3: integer;p4: integer): JFrustum;overload;
    function &set(p0: integer;p1: integer;p2: integer;p3: integer;p4: integer;p5: integer): JFrustum;overload;
    function copy(frustum: JFrustum): JFrustum;
    function setFromMatrix(m: JMatrix4): JFrustum;
    function intersectsObject(object_: JObject3D): Jboolean;
    function intersectsSphere(sphere: JSphere): Jboolean;
    function containsPoint(point: JVector3): Jboolean;
    function clone(): JFrustum;
    property planes: array of JPlane;
  end;
  JLine3 = class external "Object"
  public
    constructor Create(start: JVector3);overload;
    constructor Create(start: JVector3;&end: JVector3);overload;
    function &set(start: JVector3): JLine3;overload;
    function &set(start: JVector3;&end: JVector3): JLine3;overload;
    function copy(line: JLine3): JLine3;
    function center(optionalTarget: JVector3): JVector3;overload;
    function delta(optionalTarget: JVector3): JVector3;overload;
    function distanceSq(): integer;
    function distance(): integer;
    function at(t: integer; optionalTarget: JVector3): JVector3;overload;
    function closestPointToPointParameter(point: JVector3; clampToLine: Jboolean): integer;overload;
    function closestPointToPoint(point: JVector3; clampToLine: Jboolean): JVector3;overload;
    function closestPointToPoint(point: JVector3; clampToLine: Jboolean;optionalTarget: JVector3): JVector3;overload;
    function applyMatrix4(matrix: JMatrix4): JLine3;
    function equals(line: JLine3): Jboolean;
    function clone(): JLine3;
    property start: JVector3;
    property &end: JVector3;
  end;
  JPlane = class external "Object"
  public
    constructor Create(normal: JVector3);overload;
    constructor Create(normal: JVector3;constant: integer);overload;
    function &set(normal: JVector3; constant: integer): JPlane;
    function setComponents(x: integer; y: integer; z: integer; w: integer): JPlane;
    function setFromNormalAndCoplanarPoint(normal: JVector3; point: JVector3): JPlane;
    function setFromCoplanarPoints(a: JVector3; b: JVector3; c: JVector3): JPlane;
    function copy(plane: JPlane): JPlane;
    function normalize(): JPlane;
    function negate(): JPlane;
    function distanceToPoint(point: JVector3): integer;
    function distanceToSphere(sphere: JSphere): integer;
    function projectPoint(point: JVector3; optionalTarget: JVector3): JVector3;overload;
    function orthoPoint(point: JVector3; optionalTarget: JVector3): JVector3;overload;
    function isIntersectionLine(line: JLine3): Jboolean;
    function intersectLine(line: JLine3; optionalTarget: JVector3): JVector3;overload;
    function coplanarPoint(optionalTarget: Jboolean): JVector3;overload;
    function applyMatrix4(matrix: JMatrix4; optionalNormalMatrix: JMatrix3): JPlane;overload;
    function translate(offset: JVector3): JPlane;
    function equals(plane: JPlane): Jboolean;
    function clone(): JPlane;
    property normal: JVector3;
    property constant: integer;
  end;
  JSphere = class external "Object"
  public
    constructor Create(center: JVector3);overload;
    constructor Create(center: JVector3;radius: integer);overload;
    function &set(center: JVector3; radius: integer): JSphere;
    function setFromCenterAndPoints(center: JVector3; points: array of JVector3): JSphere;
    function copy(sphere: JSphere): JSphere;
    function empty(): Jboolean;
    function containsPoint(point: JVector3): Jboolean;
    function distanceToPoint(point: JVector3): integer;
    function intersectsSphere(sphere: JSphere): Jboolean;
    function clampPoint(point: JVector3; optionalTarget: JVector3): JVector3;overload;
    function getBoundingBox(optionalTarget: JBox3): JBox3;overload;
    function applyMatrix4(matrix: JMatrix): JSphere;
    function translate(offset: JVector3): JSphere;
    function equals(sphere: JSphere): Jboolean;
    function clone(): JSphere;
    property center: JVector3;
    property radius: integer;
  end;
  JMorphTarget = class external "Object"
    property name: string;
    property vertices: array of JVector3;
  end;
  JMorphColor = class external "Object"
    property name: string;
    property color: array of JColor;
  end;
  JBoundingBox3D = class external "Object"
    property min: JVector3;
    property max: JVector3;
  end;
  JBoundingSphere = class external "Object"
    property radius: integer;
  end;
  JGeometry = class external "Object"
  public
    constructor Create();
    procedure applyMatrix(matrix: JMatrix4);
    procedure computeCentroids();
    procedure computeFaceNormals();
    procedure computeVertexNormals(areaWeighted: Jboolean);overload;
    procedure computeMorphNormals();
    procedure computeTangents();
    procedure computeLineDistances();
    procedure computeBoundingBox();
    procedure computeBoundingSphere();
    function mergeVertices(): integer;
    function clone(): JGeometry;
    procedure dispose();
    property id: integer;
    property name: string;
    property vertices: array of JVector3;
    property colors: array of JColor;
    property normals: array of JVector3;
    property faces: array of JFace;
    property faceUvs: array of array of JVector2;
    property faceVertexUvs: array of array of array of JVector2;
    property morphTargets: array of JMorphTarget;
    property morphColors: array of JMorphColor;
    property skinWeights: array of integer;
    property skinIndices: array of integer;
    property boundingBox: JBoundingBox3D;
    property boundingSphere: JBoundingSphere;
    property hasTangents: Jboolean;
    property dynamic: Jboolean;
    property verticesNeedUpdate: Jboolean;
    property elementsNeedUpdate: Jboolean;
    property uvsNeedUpdate: Jboolean;
    property normalsNeedUpdate: Jboolean;
    property tangentsNeedUpdate: Jboolean;
    property colorsNeedUpdate: Jboolean;
    property lineDistancesNeedUpdate: Jboolean;
    property buffersNeedUpdate: Jboolean;
    property animation: JAnimationData;
  end;
  JMath = class external "Object"
  public
    function clamp(x: integer; a: integer; b: integer): integer;
    function clampBottom(x: integer; a: integer): integer;
    function mapLinear(x: integer; a1: integer; a2: integer; b1: integer; b2: integer): integer;
    function smoothstep(x: integer; min: integer; max: integer): integer;
    function smootherstep(x: integer; min: integer; max: integer): integer;
    function random16(): integer;
    function randInt(low_: integer; high_: integer): integer;
    function randFloat(low_: integer; high_: integer): integer;
    function randFloatSpread(range: integer): integer;
    function sign(x: integer): integer;
    function degToRad(degrees: integer): integer;
    function radToDeg(radians: integer): integer;
  end;
  JMatrix = class external "Object"
  public
    function identity(): JMatrix;
    function copy(m: JMatrix): JMatrix;
    function multiplyVector3Array(a: array of integer): array of integer;
    function multiplyScalar(s: integer): JMatrix;
    function determinant(): integer;
    function getInverse(matrix: JMatrix; throwOnInvertible: Jboolean): JMatrix;overload;
    function transpose(): JMatrix;
    function clone(): JMatrix;
    property elements: JFloat32Array;
  end;
  JMatrix3 = class external(JMatrix)
  public
    constructor Create();overload;
    constructor Create(n11: integer; n12: integer; n13: integer; n21: integer; n22: integer; n23: integer; n31: integer; n32: integer; n33: integer);overload;
    function &set(n11: integer; n12: integer; n13: integer; n21: integer; n22: integer; n23: integer; n31: integer; n32: integer; n33: integer): JMatrix3;
    function identity(): JMatrix3;
    function copy(m: JMatrix3): JMatrix3;
    function multiplyVector3Array(a: array of integer): array of integer;
    function multiplyScalar(s: integer): JMatrix3;
    function determinant(): integer;
    function getInverse(matrix: JMatrix3; throwOnInvertible: Jboolean): JMatrix3;overload;
    function getInverse(matrix: JMatrix4; throwOnInvertible: Jboolean): JMatrix3;overload;
    function transpose(): JMatrix3;
    function getNormalMatrix(m: JMatrix4): JMatrix3;
    function transposeIntoArray(r: array of integer): array of integer;
    function clone(): JMatrix3;
    property elements: JFloat32Array;
  end;
  JMatrix4 = class external(JMatrix)
  public
    constructor Create();overload;
    constructor Create(n11: integer; n12: integer; n13: integer; n14: integer; n21: integer; n22: integer; n23: integer; n24: integer; n31: integer; n32: integer; n33: integer; n34: integer; n41: integer; n42: integer; n43: integer; n44: integer);overload;
    function &set(n11: integer; n12: integer; n13: integer; n14: integer; n21: integer; n22: integer; n23: integer; n24: integer; n31: integer; n32: integer; n33: integer; n34: integer; n41: integer; n42: integer; n43: integer; n44: integer): JMatrix4;
    function identity(): JMatrix4;
    function copy(m: JMatrix4): JMatrix4;
    function setRotationFromEuler(v: JVector3; order: string): JMatrix4;overload;
    function setRotationFromQuaternion(q: JQuaternion): JMatrix4;
    function lookAt(eye: JVector3; target: JVector3; up: JVector3): JMatrix4;
    function multiply(m: JMatrix4): JMatrix4;
    function multiplyMatrices(a: JMatrix4; b: JMatrix4): JMatrix4;
    function multiplyToArray(a: JMatrix4; b: JMatrix4; r: array of integer): JMatrix4;
    function multiplyScalar(s: integer): JMatrix4;
    function multiplyVector3Array(a: array of integer): array of integer;
    function rotateAxis(v: JVector3): JVector3;
    function crossVector(a: JVector3): JVector4;
    function determinant(): integer;
    function transpose(): JMatrix4;
    function flattenToArray(flat: array of integer): array of integer;
    function flattenToArrayOffset(flat: array of integer; offset: integer): array of integer;
    function setPosition(v: JVector3): JVector3;
    function getInverse(m: JMatrix4; throwOnInvertible: Jboolean): JMatrix4;overload;
    function compose(translation: JVector3; rotation: JQuaternion; scale: JVector3): JMatrix4;
    function decompose(translation: JVector3): array of JObject;overload;
    function decompose(translation: JVector3;rotation: JQuaternion): array of JObject;overload;
    function decompose(translation: JVector3;rotation: JQuaternion;scale: JVector3): array of JObject;overload;
    function extractPosition(m: JMatrix4): JMatrix4;
    function extractRotation(m: JMatrix4): JMatrix4;
    function translate(v: JVector3): JMatrix4;
    function rotateX(angle: integer): JMatrix4;
    function rotateY(angle: integer): JMatrix4;
    function rotateZ(angle: integer): JMatrix4;
    function rotateByAxis(axis: JVector3; angle: integer): JMatrix4;
    function scale(v: JVector3): JMatrix4;
    function getMaxScaleOnAxis(): integer;
    function makeTranslation(x: integer; y: integer; z: integer): JMatrix4;
    function makeRotationX(theta: integer): JMatrix4;
    function makeRotationY(theta: integer): JMatrix4;
    function makeRotationZ(theta: integer): JMatrix4;
    function makeRotationAxis(axis: JVector3; angle: integer): JMatrix4;
    function makeScale(x: integer; y: integer; z: integer): JMatrix4;
    function makeFrustum(left: integer; right: integer; bottom: integer; top: integer; near: integer; far: integer): JMatrix4;
    function makePerspective(fov: integer; aspect: integer; near: integer; far: integer): JMatrix4;
    function makeOrthographic(left: integer; right: integer; top: integer; bottom: integer; near: integer; far: integer): JMatrix4;
    function clone(): JMatrix4;
    property elements: JFloat32Array;
  end;
  JRay = class external "Object"
  public
    constructor Create(origin: JVector3);overload;
    constructor Create(origin: JVector3;direction: JVector3);overload;
    function &set(origin: JVector3; direction: JVector3): JRay;
    function copy(ray: JRay): JRay;
    function at(t: integer; optionalTarget: JVector3): JVector3;overload;
    function recast(t: integer): JRay;
    function closestPointToPoint(point: JVector3; optionalTarget: JVector3): JVector3;overload;
    function distanceToPoint(point: JVector3): integer;
    function isIntersectionSphere(sphere: JSphere): Jboolean;
    function isIntersectionPlane(plane: JPlane): Jboolean;
    function distanceToPlane(plane: JPlane): integer;
    function intersectPlane(plane: JPlane; optionalTarget: JVector3): JVector3;overload;
    function applyMatrix4(matrix4: JMatrix4): JRay;
    function equals(ray: JRay): Jboolean;
    function clone(): JRay;
    property origin: JVector3;
    property direction: JVector3;
  end;

  TObject3D_traverse_callback_ = function (object_: JObject3D): variant;

  JObject3D = class external "Object"
  public
    constructor Create();
    procedure applyMatrix(matrix: JMatrix4);
    procedure translate(distance: integer; axis: JVector3);
    procedure translateX(distance: integer);
    procedure translateY(distance: integer);
    procedure translateZ(distance: integer);
    function localToWorld(vector: JVector3): JVector3;
    function worldToLocal(vector: JVector3): JVector3;
    procedure lookAt(vector: JVector3);
    procedure add(object_: JObject3D);
    procedure remove(object_: JObject3D);
    procedure traverse(callback: TObject3D_traverse_callback_);
    function getChildByName(name: string; recursive: Jboolean): JObject3D;
    function getDescendants(&array: array of JObject3D): array of JObject3D;overload;
    procedure updateMatrix();
    procedure updateMatrixWorld(force: Jboolean);
    function clone(object_: JObject3D): JObject3D;overload;
    property id: integer;
    property name: string;
    property properties: variant;
    property parent: JObject3D;
    property children: array of JObject3D;
    property position: JVector3;
    property rotation: JVector3;
    property eulerOrder: string;
    property scale: JVector3;
    property up: JVector3;
    property matrix: JMatrix4;
    property matrixRotationWorld: JMatrix4;
    property quaternion: JQuaternion;
    property useQuaternion: Jboolean;
    property boundRadius: integer;
    property boundRadiusScale: integer;
    property renderDepth: integer;
    property visible: Jboolean;
    property castShadow: Jboolean;
    property receiveShadow: Jboolean;
    property frustumCulled: Jboolean;
    property matrixAutoUpdate: Jboolean;
    property matrixWorldNeedsUpdate: Jboolean;
    property rotationAutoUpdate: Jboolean;
    property {static?} defaultEulerOrder: string;
    property userData: variant;
  end;

  TProjector_projectScene_result_object = class;

  JProjector = class external "Object"
  public
    constructor Create();
    function projectVector(vector: JVector3; camera: JCamera): JVector3;
    function unprojectVector(vector: JVector3; camera: JCamera): JVector3;
    function pickingRay(vector: JVector3; camera: JCamera): JRaycaster;
    function projectScene(scene: JScene; camera: JCamera; sortObjects: Jboolean; sortElements: Jboolean): TProjector_projectScene_result_object;overload;
  end;
  TProjector_projectScene_result_object = class external "Object"
    property objects: array of JObject3D;
    property sprites: array of JObject3D;
    property lights: array of JLight;
    property elements: array of JFace;
  end;
  JQuaternion = class external "Object"
  public
    constructor Create(x: integer);overload;
    constructor Create(x: integer;y: integer);overload;
    constructor Create(x: integer;y: integer;z: integer);overload;
    constructor Create(x: integer;y: integer;z: integer;w: integer);overload;
    function &set(x: integer; y: integer; z: integer; w: integer): JQuaternion;
    function copy(q: JQuaternion): JQuaternion;
    function setFromEuler(v: JVector3; order: string): JQuaternion;
    function setFromAxisAngle(axis: JVector3; angle: integer): JQuaternion;
    function setFromRotationMatrix(m: JMatrix4): JQuaternion;
    function inverse(): JQuaternion;
    function conjugate(): JQuaternion;
    function lengthSq(): integer;
    function length(): integer;
    function normalize(): JQuaternion;
    function multiply(q: JQuaternion): JQuaternion;
    function multiplyQuaternions(a: JQuaternion; b: JQuaternion): JQuaternion;
    function slerp(qb: JQuaternion; t: integer): JQuaternion;overload;
    function equals(v: JQuaternion): Jboolean;
    function clone(): JQuaternion;
    function slerp(qa: JQuaternion; qb: JQuaternion; qm: JQuaternion; t: integer): JQuaternion;overload;
    property x: integer;
    property y: integer;
    property z: integer;
    property w: integer;
  end;
  JIntersection = class external "Object"
    property distance: integer;
    property point: JVector3;
    property face: JFace;
    property &object: JObject3D;
  end;
  JRaycaster = class external "Object"
  public
    constructor Create(origin: JVector3);overload;
    constructor Create(origin: JVector3;direction: JVector3);overload;
    constructor Create(origin: JVector3;direction: JVector3;near: integer);overload;
    constructor Create(origin: JVector3;direction: JVector3;near: integer;far: integer);overload;
    procedure &set(origin: JVector3; direction: JVector3);
    function intersectObject(object_: JObject3D; recursive: Jboolean): array of JIntersection;overload;
    function intersectObjects(objects: array of JObject3D; recursive: Jboolean): array of JIntersection;overload;
    property ray: JRay;
    property near: integer;
    property far: integer;
    property precision: integer;
  end;
  JSplineControlPoint = class external "Object"
    property x: integer;
    property y: integer;
    property z: integer;
  end;

  TSpline_getLength_result_object = class;

  JSpline = class external "Object"
  public
    constructor Create(points: array of JSplineControlPoint);
    procedure initFromArray(a: array of array of integer);
    function getPoint(k: integer): JSplineControlPoint;
    function getControlPointsArray(): array of array of integer;
    function getLength(nSubDivisions: integer): TSpline_getLength_result_object;overload;
    procedure reparametrizeByArcLength(samplingCoef: integer);
    property points: array of JSplineControlPoint;
  end;
  TSpline_getLength_result_object = class external "Object"
    property chunks: array of integer;
    property total: integer;
  end;
  JTriangle = class external "Object"
  public
    constructor Create(a: JVector3);overload;
    constructor Create(a: JVector3;b: JVector3);overload;
    constructor Create(a: JVector3;b: JVector3;c: JVector3);overload;
    function &set(a: JVector3; b: JVector3; c: JVector3): JTriangle;
    function setFromPointsAndIndices(points: array of JVector3; i0: integer; i1: integer; i2: integer): JTriangle;
    function copy(triangle: JTriangle): JTriangle;
    function area(): integer;
    function midpoint(optionalTarget: JVector3): JVector3;overload;
    function normal(optionalTarget: JVector3): JVector3;overload;
    function plane(optionalTarget: JVector3): JPlane;overload;
    function barycoordFromPoint(point: JVector3; optionalTarget: JVector3): JVector3;overload;
    function containsPoint(point: JVector3): Jboolean;overload;
    function equals(triangle: JTriangle): Jboolean;
    function clone(): JTriangle;
    function normal(a: JVector3; b: JVector3; c: JVector3; optionalTarget: JVector3): JVector3;overload;
    function barycoordFromPoint(point: JVector3; a: JVector3; b: JVector3; c: JVector3; optionalTarget: JVector3): JVector3;overload;
    function containsPoint(point: JVector3; a: JVector3; b: JVector3; c: JVector3): Jboolean;overload;
    property a: JVector3;
    property b: JVector3;
    property c: JVector3;
  end;
  JVector = class external "Object"
  public
    procedure setComponent(index: integer; value: integer);
    function getComponent(index: integer): integer;
    function copy(v: JVector): JVector;
    function add(v: JVector): JVector;
    function addVectors(a: JVector; b: JVector): JVector;
    function sub(v: JVector): JVector;
    function subVectors(a: JVector; b: JVector): JVector;
    function multiplyScalar(s: integer): JVector;
    function divideScalar(s: integer): JVector;
    function negate(): JVector;
    function dot(v: JVector): integer;
    function lengthSq(): integer;
    function length(): integer;
    function normalize(): JVector;
    function distanceTo(v: JVector): integer;
    function distanceToSquared(v: JVector): integer;
    function setLength(l: integer): JVector;
    function lerp(v: JVector; alpha: integer): JVector;
    function equals(v: JVector): Jboolean;
    function clone(): JVector;
  end;
  JVector2 = class external(JVector)
  public
    constructor Create(x: integer);overload;
    constructor Create(x: integer;y: integer);overload;
    function &set(x: integer; y: integer): JVector2;
    function setX(x: integer): JVector2;
    function setY(y: integer): JVector2;
    procedure setComponent(index: integer; value: integer);
    function getComponent(index: integer): integer;
    function copy(v: JVector2): JVector2;
    function add(v: JVector2): JVector2;
    function addVectors(a: JVector2; b: JVector2): JVector2;
    function sub(v: JVector2): JVector2;
    function subVectors(a: JVector2; b: JVector2): JVector2;
    function multiplyScalar(s: integer): JVector2;
    function divideScalar(s: integer): JVector2;
    function min(v: JVector2): JVector2;
    function max(v: JVector2): JVector2;
    function clamp(min: JVector2; max: JVector2): JVector2;
    function negate(): JVector2;
    function dot(v: JVector2): integer;
    function lengthSq(): integer;
    function length(): integer;
    function normalize(): JVector2;
    function distanceTo(v: JVector2): integer;
    function distanceToSquared(v: JVector2): integer;
    function setLength(l: integer): JVector2;
    function lerp(v: JVector2; alpha: integer): JVector2;
    function equals(v: JVector2): Jboolean;
    function toArray(): array of integer;
    function clone(): JVector2;
    property x: integer;
    property y: integer;
  end;
  JVector3 = class external(JVector)
  public
    constructor Create(x: integer);overload;
    constructor Create(x: integer;y: integer);overload;
    constructor Create(x: integer;y: integer;z: integer);overload;
    function &set(x: integer; y: integer; z: integer): JVector3;
    function setX(x: integer): JVector3;
    function setY(y: integer): JVector3;
    function setZ(z: integer): JVector3;
    procedure setComponent(index: integer; value: integer);
    function getComponent(index: integer): integer;
    function copy(v: JVector3): JVector3;
    function add(a: JObject): JVector3;
    function addScalar(s: integer): JVector3;
    function addVectors(a: JVector3; b: JVector3): JVector3;
    function sub(a: JVector3): JVector3;
    function subVectors(a: JVector3; b: JVector3): JVector3;
    function multiply(v: JVector3): JVector3;
    function multiplyScalar(s: integer): JVector3;
    function multiplyVectors(a: JVector3; b: JVector3): JVector3;
    function applyMatrix3(m: JMatrix3): JVector3;
    function applyMatrix4(m: JMatrix4): JVector3;
    function applyProjection(m: JMatrix4): JVector3;
    function applyQuaternion(q: JQuaternion): JVector3;
    function applyEuler(v: JVector3; eulerOrder: string): JVector3;
    function applyAxisAngle(axis: JVector3; angle: integer): JVector3;
    function transformDirection(m: JMatrix4): JVector3;
    function divide(v: JVector3): JVector3;
    function divideScalar(s: integer): JVector3;
    function min(v: JVector3): JVector3;
    function max(v: JVector3): JVector3;
    function clamp(min: JVector3; max: JVector3): JVector3;
    function negate(): JVector3;
    function dot(v: JVector3): integer;
    function lengthSq(): integer;
    function length(): integer;
    function lengthManhattan(): integer;
    function normalize(): JVector3;
    function setLength(l: integer): JVector3;
    function lerp(v: JVector3; alpha: integer): JVector3;
    function cross(a: JVector3): JVector3;
    function crossVectors(a: JVector3; b: JVector3): JVector3;
    function projectOnVector(v: JVector3): JVector3;
    function projectOnPlane(planeNormal: JVector3): JVector3;
    function reflect(vector: JVector3): JVector3;
    function angleTo(v: JVector3): integer;
    function distanceTo(v: JVector3): integer;
    function distanceToSquared(v: JVector3): integer;
    function getPositionFromMatrix(m: JMatrix4): JVector3;
    function setEulerFromRotationMatrix(m: JMatrix4; order: string): JVector3;
    function setEulerFromQuaternion(q: JQuaternion; order: string): JVector3;
    function getScaleFromMatrix(m: JMatrix4): JVector3;
    function equals(v: JVector3): Jboolean;
    function toArray(): array of integer;
    function clone(): JVector3;
    property x: float;
    property y: float;
    property z: float;
  end;
  JVector4 = class external(JVector)
  public
    constructor Create(x: integer);overload;
    constructor Create(x: integer;y: integer);overload;
    constructor Create(x: integer;y: integer;z: integer);overload;
    constructor Create(x: integer;y: integer;z: integer;w: integer);overload;
    function &set(x: integer; y: integer; z: integer; w: integer): JVector4;
    function setX(x: integer): JVector2;
    function setY(y: integer): JVector2;
    function setZ(z: integer): JVector2;
    function setW(w: integer): JVector2;
    procedure setComponent(index: integer; value: integer);
    function getComponent(index: integer): integer;
    function copy(v: JVector4): JVector4;
    function add(v: JVector4): JVector4;
    function addVectors(a: JVector4; b: JVector4): JVector4;
    function sub(v: JVector4): JVector4;
    function subVectors(a: JVector4; b: JVector4): JVector4;
    function multiplyScalar(s: integer): JVector4;
    function divideScalar(s: integer): JVector4;
    function min(v: JVector4): JVector4;
    function max(v: JVector4): JVector4;
    function clamp(min: JVector4; max: JVector4): JVector4;
    function negate(): JVector4;
    function dot(v: JVector4): integer;
    function lengthSq(): integer;
    function length(): integer;
    function lengthManhattan(): integer;
    function normalize(): JVector4;
    function distanceTo(v: JVector3): integer;
    function distanceToSquared(v: JVector3): integer;
    function setLength(l: integer): JVector4;
    function lerp(v: JVector4; alpha: integer): JVector4;
    function equals(v: JVector4): Jboolean;
    function toArray(): array of integer;
    function clone(): JVector4;
    function setAxisAngleFromQuaternion(q: JQuaternion): JVector4;
    function setAxisAngleFromRotationMatrix(m: JMatrix3): JVector4;
    property x: integer;
    property y: integer;
    property z: integer;
    property w: integer;
  end;
  JBox2 = class external "Object"
  public
    constructor Create(min: JVector2);overload;
    constructor Create(min: JVector2;max: JVector2);overload;
    function &set(min: JVector2; max: JVector2): JBox2;
    function setFromPoints(points: array of JVector2): JBox2;
    function setFromCenterAndSize(center: JVector2; size: integer): JBox2;
    function copy(box: JBox2): JBox2;
    function makeEmpty(): JBox2;
    function empty(): Jboolean;
    function center(optionalTarget: JVector2): JVector2;overload;
    function size(optionalTarget: JVector2): JVector2;overload;
    function expandByPoint(point: JVector2): JBox2;
    function expandByVector(vector: JVector2): JBox2;
    function expandByScalar(scalar: integer): JBox2;
    function containsPoint(point: JVector2): Jboolean;
    function containsBox(box: JBox2): Jboolean;
    function getParameter(point: JVector2): JVector2;
    function isIntersectionBox(box: JBox2): Jboolean;
    function clampPoint(point: JVector2; optionalTarget: JVector2): JVector2;overload;
    function distanceToPoint(point: JVector2): integer;
    function intersect(box: JBox2): JBox2;
    function union(box: JBox2): JBox2;
    function translate(offset: JVector2): JBox2;
    function equals(box: JBox2): Jboolean;
    function clone(): JBox2;
    property min: JVector2;
    property max: JVector2;
  end;
  JBox3 = class external "Object"
  public
    constructor Create(min: JVector3);overload;
    constructor Create(min: JVector3;max: JVector3);overload;
    function &set(min: JVector3; max: JVector3): JBox3;
    function setFromPoints(points: array of JVector3): JBox3;
    function setFromCenterAndSize(center: JVector3; size: integer): JBox3;
    function copy(box: JBox3): JBox3;
    function makeEmpty(): JBox3;
    function empty(): Jboolean;
    function center(optionalTarget: JVector3): JVector3;overload;
    function size(optionalTarget: JVector3): JVector3;overload;
    function expandByPoint(point: JVector3): JBox3;
    function expandByVector(vector: JVector3): JBox3;
    function expandByScalar(scalar: integer): JBox3;
    function containsPoint(point: JVector3): Jboolean;
    function containsBox(box: JBox3): Jboolean;
    function getParameter(point: JVector3): JVector3;
    function isIntersectionBox(box: JBox3): Jboolean;
    function clampPoint(point: JVector3; optionalTarget: JVector3): JVector3;overload;
    function distanceToPoint(point: JVector3): integer;
    function getBoundingSphere(): JSphere;
    function intersect(box: JBox3): JBox3;
    function union(box: JBox3): JBox3;
    function applyMatrix4(matrix: JMatrix4): JBox3;
    function translate(offset: JVector3): JBox3;
    function equals(box: JBox3): Jboolean;
    function clone(): JBox3;
    property min: JVector3;
    property max: JVector3;
  end;
  JCamera = class external(JObject3D)
  public
    constructor Create();
    procedure lookAt(vector: JVector3);
    property matrixWorldInverse: JMatrix4;
    property projectionMatrix: JMatrix4;
    property projectionMatrixInverse: JMatrix4;
  end;
  JOrthographicCamera = class external(JCamera)
  public
    constructor Create(left: integer; right: integer; top: integer; bottom: integer; near: integer);overload;
    constructor Create(left: integer; right: integer; top: integer; bottom: integer; near: integer;far: integer);overload;
    procedure updateProjectionMatrix();
    property left: integer;
    property right: integer;
    property top: integer;
    property bottom: integer;
    property near: integer;
    property far: integer;
  end;
  JPerspectiveCamera = class external 'THREE.PerspectiveCamera' (JCamera)
  public
    constructor Create(fov: integer);overload;
    constructor Create(fov: integer;aspect: integer);overload;
    constructor Create(fov: integer;aspect: integer;near: integer);overload;
    constructor Create(fov: integer;aspect: integer;near: integer;far: integer);overload;
    procedure setLens(focalLength: integer; frameHeight: integer);overload;
    procedure setViewOffset(fullWidth: integer; fullHeight: integer; x: integer; y: integer; width: integer; height: integer);
    procedure updateProjectionMatrix();
    property fov: integer;
    property aspect: integer;
    property near: integer;
    property far: integer;
  end;
  JLight = class external(JObject3D)
  public
    constructor Create(hex: integer);overload;
    property color: JColor;
  end;
  JAmbientLight = class external(JLight)
  public
    constructor Create(hex: integer);overload;
  end;
  JAreaLight = class external "Object"
  public
    constructor Create(hex: integer; intensity: integer);overload;
    property normal: JVector3;
    property right: JVector3;
    property intensity: integer;
    property width: integer;
    property height: integer;
    property constantAttenuation: integer;
    property linearAttenuation: integer;
    property quadraticAttenuation: integer;
  end;
  JDirectionalLight = class external(JLight)
  public
    constructor Create(hex: integer);overload;
    constructor Create(hex: integer;intensity: integer);overload;
    property position: JVector3;
    property target: JObject3D;
    property intensity: integer;
    property castShadow: Jboolean;
    property onlyShadow: Jboolean;
    property shadowCameraNear: integer;
    property shadowCameraFar: integer;
    property shadowCameraLeft: integer;
    property shadowCameraRight: integer;
    property shadowCameraTop: integer;
    property shadowCameraBottom: integer;
    property shadowCameraVisible: Jboolean;
    property shadowBias: integer;
    property shadowDarkness: integer;
    property shadowMapWidth: integer;
    property shadowMapHeight: integer;
    property shadowCascade: Jboolean;
    property shadowCascadeOffset: JVector3;
    property shadowCascadeCount: integer;
    property shadowCascadeBias: array of integer;
    property shadowCascadeWidth: array of integer;
    property shadowCascadeHeight: array of integer;
    property shadowCascadeNearZ: array of integer;
    property shadowCascadeFarZ: array of integer;
    property shadowCascadeArray: array of JDirectionalLight;
    property shadowMap: JRenderTarget;
    property shadowMapSize: integer;
    property shadowCamera: JCamera;
    property shadowMatrix: JMatrix4;
  end;
  JHemisphereLight = class external(JLight)
  public
    constructor Create(skyColorHex: integer);overload;
    constructor Create(skyColorHex: integer;groundColorHex: integer);overload;
    constructor Create(skyColorHex: integer;groundColorHex: integer;intensity: integer);overload;
    property groundColor: JColor;
    property position: JVector3;
    property intensity: integer;
  end;
  JPointLight = class external(JLight)
  public
    constructor Create(hex: integer);overload;
    constructor Create(hex: integer;intensity: integer);overload;
    constructor Create(hex: integer;intensity: integer;distance: integer);overload;
    property position: JVector3;
    property intensity: integer;
    property distance: integer;
  end;
  JSpotLight = class external(JLight)
  public
    constructor Create(hex: integer);overload;
    constructor Create(hex: integer;intensity: integer);overload;
    constructor Create(hex: integer;intensity: integer;distance: integer);overload;
    constructor Create(hex: integer;intensity: integer;distance: integer;angle: integer);overload;
    constructor Create(hex: integer;intensity: integer;distance: integer;angle: integer;exponent: integer);overload;
    property position: JVector3;
    property target: JObject3D;
    property intensity: integer;
    property distance: integer;
    property angle: integer;
    property exponent: integer;
    property castShadow: Jboolean;
    property onlyShadow: Jboolean;
    property shadowCameraNear: integer;
    property shadowCameraFar: integer;
    property shadowCameraFov: integer;
    property shadowCameraVisible: Jboolean;
    property shadowBias: integer;
    property shadowDarkness: integer;
    property shadowMapWidth: integer;
    property shadowMapHeight: integer;
    property shadowMap: JRenderTarget;
    property shadowMapSize: JVector2;
    property shadowCamera: JCamera;
    property shadowMatrix: JMatrix4;
  end;
  JProgress = class external "Object"
    property total: integer;
    property loaded: integer;
  end;

  TLoader_onLoadStart_ = procedure ();
  TLoader_onLoadProgress_ = procedure ();
  TLoader_onLoadComplete_ = procedure ();

  JLoader = class external "Object"
  public
    constructor Create(showStatus: Jboolean);overload;
    function addStatusElement(): JHTMLElement;
    procedure updateProgress(progress: JProgress);
    function extractUrlBase(url: string): string;
    function initMaterials(materials: array of JMaterial; texturePath: string): array of JMaterial;
    function needsTangents(materials: array of JMaterial): Jboolean;
    function createMaterial(m: JMaterial; texturePath: string): Jboolean;
    property showStatus: Jboolean;
    property statusDomElement: JHTMLElement;
    property onLoadStart: TLoader_onLoadStart_;
    property onLoadProgress: TLoader_onLoadProgress_;
    property onLoadComplete: TLoader_onLoadComplete_;
    property crossOrigin: string;
  end;
  JImageLoader = class external(JEventDispatcher)
  public
    constructor Create();
    procedure load(url: string; image: JHTMLImageElement);overload;
    property crossOrigin: string;
  end;

  TJSONLoader_load_callback_ = procedure (geometry: JGeometry; materials: array of JMaterial);
  TJSONLoader_loadAjaxJSON_callback_ = procedure (geometry: JGeometry; materials: array of JMaterial);
  TJSONLoader_loadAjaxJSON_callbackProgress_ = procedure (progress: JProgress);
  TJSONLoader_createModel_callback_ = procedure (geometry: JGeometry; materials: array of JMaterial);

  JJSONLoader = class external(JLoader)
  public
    constructor Create(showStatus: Jboolean);overload;
    procedure load(url: string; callback: TJSONLoader_load_callback_; texturePath: string);overload;
    procedure loadAjaxJSON(context: JJSONLoader; url: string; callback: TJSONLoader_loadAjaxJSON_callback_; texturePath: string);overload;
    procedure loadAjaxJSON(context: JJSONLoader; url: string; callback: TJSONLoader_loadAjaxJSON_callback_; texturePath: string;callbackProgress: TJSONLoader_loadAjaxJSON_callbackProgress_);overload;
    procedure createModel(json: variant; callback: TJSONLoader_createModel_callback_; texturePath: string);overload;
    property withCredentials: Jboolean;
  end;
  JLoadingMonitor = class external(JEventDispatcher)
  public
    constructor Create();
    procedure add(loader: JLoader);
  end;

  TSceneLoaderResult_geometries_object = class;
  TSceneLoaderResult_face_materials_object = class;
  TSceneLoaderResult_materials_object = class;
  TSceneLoaderResult_textures_object = class;
  TSceneLoaderResult_objects_object = class;
  TSceneLoaderResult_cameras_object = class;
  TSceneLoaderResult_lights_object = class;
  TSceneLoaderResult_fogs_object = class;
  TSceneLoaderResult_empties_object = class;
  TSceneLoaderResult_groups_object = class;

  JSceneLoaderResult = class external "Object"
    property scene: JScene;
    property geometries: TSceneLoaderResult_geometries_object;
    property face_materials: TSceneLoaderResult_face_materials_object;
    property materials: TSceneLoaderResult_materials_object;
    property textures: TSceneLoaderResult_textures_object;
    property objects: TSceneLoaderResult_objects_object;
    property cameras: TSceneLoaderResult_cameras_object;
    property lights: TSceneLoaderResult_lights_object;
    property fogs: TSceneLoaderResult_fogs_object;
    property empties: TSceneLoaderResult_empties_object;
    property groups: TSceneLoaderResult_groups_object;
  end;
  TSceneLoaderResult_geometries_object = class external "Object"
  public
    function  GetItems(id: string): JGeometry; external array;
    procedure SetItems(id: string; value: JGeometry); external array;
    property Items[id: string]: JGeometry read GetItems write SetItems; default;
  end;
  TSceneLoaderResult_face_materials_object = class external "Object"
  public
    function  GetItems(id: string): JMaterial; external array;
    procedure SetItems(id: string; value: JMaterial); external array;
    property Items[id: string]: JMaterial read GetItems write SetItems; default;
  end;
  TSceneLoaderResult_materials_object = class external "Object"
  public
    function  GetItems(id: string): JMaterial; external array;
    procedure SetItems(id: string; value: JMaterial); external array;
    property Items[id: string]: JMaterial read GetItems write SetItems; default;
  end;
  TSceneLoaderResult_textures_object = class external "Object"
  public
    function  GetItems(id: string): JTexture; external array;
    procedure SetItems(id: string; value: JTexture); external array;
    property Items[id: string]: JTexture read GetItems write SetItems; default;
  end;
  TSceneLoaderResult_objects_object = class external "Object"
  public
    function  GetItems(id: string): JObject3D; external array;
    procedure SetItems(id: string; value: JObject3D); external array;
    property Items[id: string]: JObject3D read GetItems write SetItems; default;
  end;
  TSceneLoaderResult_cameras_object = class external "Object"
  public
    function  GetItems(id: string): JCamera; external array;
    procedure SetItems(id: string; value: JCamera); external array;
    property Items[id: string]: JCamera read GetItems write SetItems; default;
  end;
  TSceneLoaderResult_lights_object = class external "Object"
  public
    function  GetItems(id: string): JLight; external array;
    procedure SetItems(id: string; value: JLight); external array;
    property Items[id: string]: JLight read GetItems write SetItems; default;
  end;
  TSceneLoaderResult_fogs_object = class external "Object"
  public
    function  GetItems(id: string): JIFog; external array;
    procedure SetItems(id: string; value: JIFog); external array;
    property Items[id: string]: JIFog read GetItems write SetItems; default;
  end;
  TSceneLoaderResult_empties_object = class external "Object"
  public
    function  GetItems(id: string): variant; external array;
    procedure SetItems(id: string; value: variant); external array;
    property Items[id: string]: variant read GetItems write SetItems; default;
  end;
  TSceneLoaderResult_groups_object = class external "Object"
  public
    function  GetItems(id: string): variant; external array;
    procedure SetItems(id: string; value: variant); external array;
    property Items[id: string]: variant read GetItems write SetItems; default;
  end;
  JSceneLoaderProgress = class external "Object"
    property totalModels: integer;
    property totalTextures: integer;
    property loadedModels: integer;
    property loadedTextures: integer;
  end;

  TSceneLoader_onLoadStart_ = procedure ();
  TSceneLoader_onLoadProgress_ = procedure ();
  TSceneLoader_onLoadComplete_ = procedure ();
  TSceneLoader_callbackSync_ = procedure (result: JSceneLoaderResult);
  TSceneLoader_callbackProgress_ = procedure (progress: JSceneLoaderProgress; result: JSceneLoaderResult);
  TSceneLoader_load_callbackFinished_ = procedure (scene: JScene);
  TSceneLoader_parse_callbackFinished_ = procedure (result: JSceneLoaderResult);

  JSceneLoader = class external "Object"
  public
    constructor Create();
    procedure load(url: string; callbackFinished: TSceneLoader_load_callbackFinished_);
    procedure addGeometryHandler(typeID: string; loaderClass: JObject);
    procedure addHierarchyHandler(typeID: string; loaderClass: JObject);
    procedure parse(json: variant; callbackFinished: TSceneLoader_parse_callbackFinished_; url: string);
    property onLoadStart: TSceneLoader_onLoadStart_;
    property onLoadProgress: TSceneLoader_onLoadProgress_;
    property onLoadComplete: TSceneLoader_onLoadComplete_;
    property callbackSync: TSceneLoader_callbackSync_;
    property callbackProgress: TSceneLoader_callbackProgress_;
    property geometryHandlerMap: variant;
    property hierarchyHandlerMap: variant;
  end;
  JTextureLoader = class external(JEventDispatcher)
  public
    constructor Create();
    procedure load(url: string);
  end;
  JMaterial = class external "Object"
  public
    constructor Create();
    function clone(): JMaterial;
    procedure dispose();
    property id: integer;
    property name: string;
    property opacity: integer;
    property transparent: Jboolean;
    property blending: JBlending;
    property blendSrc: JBlendingDstFactor;
    property blendDst: JBlendingSrcFactor;
    property blendEquation: JBlendingEquation;
    property depthTest: Jboolean;
    property depthWrite: Jboolean;
    property polygonOffset: Jboolean;
    property polygonOffsetFactor: integer;
    property polygonOffsetUnits: integer;
    property alphaTest: integer;
    property overdraw: Jboolean;
    property visible: Jboolean;
    property side: JSide;
    property needsUpdate: Jboolean;
  end;
  JLineBasicMaterialParameters = class external "Object"
    property color: integer;
    property opacity: integer;
    property blending: JBlending;
    property depthTest: Jboolean;
    property linewidth: integer;
    property linecap: string;
    property linejoin: string;
    property vertexColors: JColors;
    property fog: Jboolean;
  end;
  JLineBasicMaterial = class external(JMaterial)
  public
    constructor Create(parameters: JLineBasicMaterialParameters);overload;
    function clone(): JLineBasicMaterial;
    property color: JColor;
    property linewidth: integer;
    property linecap: string;
    property linejoin: string;
    property vertexColors: Jboolean;
    property fog: Jboolean;
  end;
  JLineDashedMaterialParameters = class external "Object"
    property color: integer;
    property opacity: integer;
    property blending: JBlending;
    property depthTest: Jboolean;
    property linewidth: integer;
    property scale: integer;
    property dashSize: integer;
    property gapSize: integer;
    property vertexColors: integer;
    property fog: Jboolean;
  end;
  JLineDashedMaterial = class external(JMaterial)
  public
    constructor Create(parameters: JLineDashedMaterialParameters);overload;
    function clone(): JLineDashedMaterial;
    property color: JColor;
    property linewidth: integer;
    property scale: integer;
    property dashSize: integer;
    property gapSize: integer;
    property vertexColors: Jboolean;
    property fog: Jboolean;
  end;
  JMeshBasicMaterialParameters = class external "Object"
    property color: integer;
    property opacity: integer;
    property map: JTexture;
    property lightMap: JTexture;
    property specularMap: JTexture;
    property envMap: JTexture;
    property combine: JCombine;
    property reflectivity: integer;
    property refractionRatio: integer;
    property shading: JShading;
    property blending: JBlending;
    property depthTest: Jboolean;
    property wireframe: Jboolean;
    property wireframeLinewidth: integer;
    property vertexColors: JColors;
    property skinning: Jboolean;
    property morphTargets: Jboolean;
    property fog: Jboolean;
  end;
  JMeshBasicMaterial = class external 'THREE.MeshBasicMaterial' (JMaterial)
  public
    constructor Create(parameters: JMeshBasicMaterialParameters);overload;
    function clone(): JMeshBasicMaterial;
    property color: JColor;
    property map: JTexture;
    property lightMap: JTexture;
    property specularMap: JTexture;
    property envMap: JTexture;
    property combine: JCombine;
    property reflectivity: integer;
    property refractionRatio: integer;
    property fog: Jboolean;
    property shading: JShading;
    property wireframe: Jboolean;
    property wireframeLinewidth: integer;
    property wireframeLinecap: string;
    property wireframeLinejoin: string;
    property vertexColors: JColors;
    property skinning: Jboolean;
    property morphTargets: Jboolean;
  end;
  JMeshDepthMaterialParameters = class external "Object"
    property opacity: integer;
    property blending: JBlending;
    property depthTest: Jboolean;
    property wireframe: Jboolean;
    property wireframeLinewidth: integer;
  end;
  JMeshDepthMaterial = class external(JMaterial)
  public
    constructor Create(parameters: JMeshDepthMaterialParameters);overload;
    function clone(): JMeshDepthMaterial;
    property wireframe: Jboolean;
    property wireframeLinewidth: integer;
  end;
  JMeshFaceMaterial = class external(JMaterial)
  public
    constructor Create(materials: array of JMaterial);overload;
    function clone(): JMeshFaceMaterial;
    property materials: array of JMaterial;
  end;
  JMeshLambertMaterialParameters = class external "Object"
    property color: integer;
    property ambient: integer;
    property emissive: integer;
    property opacity: integer;
    property map: JTexture;
    property lightMap: JTexture;
    property specularMap: JTexture;
    property envMap: JTexture;
    property combine: JCombine;
    property reflectivity: integer;
    property refractionRatio: integer;
    property shading: JShading;
    property blending: JBlending;
    property depthTest: Jboolean;
    property wireframe: Jboolean;
    property wireframeLinewidth: integer;
    property vertexColors: JColors;
    property skinning: Jboolean;
    property morphTargets: Jboolean;
    property morphNormals: Jboolean;
    property fog: Jboolean;
  end;
  JMeshLambertMaterial = class external(JMaterial)
  public
    constructor Create(parameters: JMeshLambertMaterialParameters);overload;
    function clone(): JMeshLambertMaterial;
    property color: JColor;
    property ambient: JColor;
    property emissive: JColor;
    property wrapAround: Jboolean;
    property wrapRGB: JVector3;
    property map: JTexture;
    property lightMap: JTexture;
    property specularMap: JTexture;
    property envMap: JTexture;
    property combine: JCombine;
    property reflectivity: integer;
    property refractionRatio: integer;
    property fog: Jboolean;
    property shading: JShading;
    property wireframe: Jboolean;
    property wireframeLinewidth: integer;
    property wireframeLinecap: string;
    property wireframeLinejoin: string;
    property vertexColors: JColors;
    property skinning: Jboolean;
    property morphTargets: Jboolean;
    property morphNormals: Jboolean;
  end;
  JMeshNormalMaterialParameters = class external "Object"
    property opacity: integer;
    property shading: JShading;
    property blending: JBlending;
    property depthTest: Jboolean;
    property wireframe: Jboolean;
    property wireframeLinewidth: integer;
  end;
  JMeshNormalMaterial = class external(JMaterial)
  public
    constructor Create(parameters: JMeshNormalMaterialParameters);overload;
    function clone(): JMeshNormalMaterial;
    property shading: JShading;
    property wireframe: Jboolean;
    property wireframeLinewidth: integer;
  end;
  JMeshPhongMaterialParameters = class external "Object"
    property color: integer;
    property ambient: integer;
    property emissive: integer;
    property specular: integer;
    property shininess: integer;
    property opacity: integer;
    property map: JTexture;
    property lightMap: JTexture;
    property bumpMap: JTexture;
    property bumpScale: integer;
    property normalMap: JTexture;
    property normalScale: JVector2;
    property specularMap: JTexture;
    property envMap: JTexture;
    property combine: JCombine;
    property reflectivity: integer;
    property refractionRatio: integer;
    property shading: JShading;
    property blending: JBlending;
    property depthTest: Jboolean;
    property wireframe: Jboolean;
    property wireframeLinewidth: integer;
    property vertexColors: JColors;
    property skinning: Jboolean;
    property morphTargets: Jboolean;
    property morphNormals: Jboolean;
    property fog: Jboolean;
  end;
  JMeshPhongMaterial = class external(JMaterial)
  public
    constructor Create(parameters: JMeshPhongMaterialParameters);overload;
    function clone(): JMeshPhongMaterial;
    property color: JColor;
    property ambient: JColor;
    property emissive: JColor;
    property specular: JColor;
    property shininess: integer;
    property metal: Jboolean;
    property perPixel: Jboolean;
    property wrapAround: Jboolean;
    property wrapRGB: JVector3;
    property map: JTexture;
    property lightMap: JTexture;
    property bumpMap: JTexture;
    property bumpScale: integer;
    property normalMap: JTexture;
    property normalScale: JVector2;
    property specularMap: JTexture;
    property envMap: JTexture;
    property combine: JCombine;
    property reflectivity: integer;
    property refractionRatio: integer;
    property fog: Jboolean;
    property shading: JShading;
    property wireframe: Jboolean;
    property wireframeLinewidth: integer;
    property wireframeLinecap: string;
    property wireframeLinejoin: string;
    property vertexColors: JColors;
    property skinning: Jboolean;
    property morphTargets: Jboolean;
    property morphNormals: Jboolean;
  end;
  JParticleBasicMaterialParameters = class external "Object"
    property color: integer;
    property opacity: integer;
    property map: JTexture;
    property size: integer;
    property blending: JBlending;
    property depthTest: Jboolean;
    property vertexColors: Jboolean;
    property fog: Jboolean;
  end;
  JParticleBasicMaterial = class external(JMaterial)
  public
    constructor Create(parameters: JParticleBasicMaterialParameters);overload;
    function clone(): JParticleBasicMaterial;
    property color: JColor;
    property map: JTexture;
    property size: integer;
    property sizeAttenuation: Jboolean;
    property vertexColors: Jboolean;
    property fog: Jboolean;
  end;

  TParticleCanvasMaterialParameters_program_ = procedure (context: JCanvasRenderingContext2D; color: JColor);

  JParticleCanvasMaterialParameters = class external "Object"
    property color: integer;
    property program: TParticleCanvasMaterialParameters_program_;
    property opacity: integer;
    property blending: JBlending;
  end;

  TParticleCanvasMaterial_program_ = procedure (context: JCanvasRenderingContext2D; color: JColor);

  JParticleCanvasMaterial = class external(JMaterial)
  public
    constructor Create(parameters: JParticleCanvasMaterialParameters);overload;
    function clone(): JParticleCanvasMaterial;
    property color: JColor;
    property program: TParticleCanvasMaterial_program_;
  end;
  JParticleDOMMaterial = class external(JMaterial)
  public
    constructor Create(element: JHTMLElement);
    function clone(): JParticleDOMMaterial;
  end;

  TUniforms_Items_result_object = class;
  TUniforms_color_object = class;

  JUniforms = class external "Object"
  public
    function  GetItems(name: string): TUniforms_Items_result_object; external array;
    procedure SetItems(name: string; value: TUniforms_Items_result_object); external array;
    property Items[name: string]: TUniforms_Items_result_object read GetItems write SetItems; default;
    property color: TUniforms_color_object;
  end;
  TUniforms_Items_result_object = class external "Object"
    property &type: string;
    property value: variant;
  end;
  TUniforms_color_object = class external "Object"
    property &type: string;
    property value: JColor;
  end;

  TShaderMaterialParameters_defines_object = class;

  JShaderMaterialParameters = class external "Object"
    property fragmentShader: string;
    property vertexShader: string;
    property uniforms: JUniforms;
    property defines: TShaderMaterialParameters_defines_object;
    property shading: JShading;
    property blending: JBlending;
    property depthTest: Jboolean;
    property wireframe: Jboolean;
    property wireframeLinewidth: integer;
    property lights: Jboolean;
    property vertexColors: JColors;
    property skinning: Jboolean;
    property morphTargets: Jboolean;
    property morphNormals: Jboolean;
    property fog: Jboolean;
  end;
  TShaderMaterialParameters_defines_object = class external "Object"
  public
    function  GetItems(label: string): string; external array;
    procedure SetItems(label: string; value: string); external array;
    property Items[label: string]: string read GetItems write SetItems; default;
  end;

  TShaderMaterial_defines_object = class;
  TShaderMaterial_attributes_object = class;

  JShaderMaterial = class external(JMaterial)
  public
    constructor Create(parameters: JShaderMaterialParameters);overload;
    function clone(): JShaderMaterial;
    property fragmentShader: string;
    property vertexShader: string;
    property uniforms: JUniforms;
    property defines: TShaderMaterial_defines_object;
    property attributes: TShaderMaterial_attributes_object;
    property shading: JShading;
    property wireframe: Jboolean;
    property wireframeLinewidth: integer;
    property fog: Jboolean;
    property lights: Jboolean;
    property vertexColors: JColors;
    property skinning: Jboolean;
    property morphTargets: Jboolean;
    property morphNormals: Jboolean;
  end;
  TShaderMaterial_defines_object = class external "Object"
  public
    function  GetItems(label: string): string; external array;
    procedure SetItems(label: string; value: string); external array;
    property Items[label: string]: string read GetItems write SetItems; default;
  end;
  TShaderMaterial_attributes_object = class external "Object"
  public
    function  GetItems(name: string): JObject; external array;
    procedure SetItems(name: string; value: JObject); external array;
    property Items[name: string]: JObject read GetItems write SetItems; default;
  end;
  JBone = class external(JObject3D)
  public
    constructor Create(belongsToSkin: JSkinnedMesh);
    procedure update(parentSkinMatrix: JMatrix4);overload;
    procedure update(parentSkinMatrix: JMatrix4;forceUpdate: Jboolean);overload;
    property skin: JSkinnedMesh;
    property skinMatrix: JMatrix4;
  end;

TLineType = variant;

  JLine = class external(JObject3D)
  public
    constructor Create(geometry: JGeometry);overload;
    constructor Create(geometry: JGeometry;material: JLineDashedMaterial);overload;
    constructor Create(geometry: JGeometry;material: JLineDashedMaterial;&type: integer);overload;
    constructor Create(geometry: JGeometry;material: JLineBasicMaterial);overload;
    constructor Create(geometry: JGeometry;material: JLineBasicMaterial;&type: integer);overload;
    constructor Create(geometry: JGeometry;material: JShaderMaterial);overload;
    constructor Create(geometry: JGeometry;material: JShaderMaterial;&type: integer);overload;
    constructor Create(geometry: JBufferGeometry);overload;
    constructor Create(geometry: JBufferGeometry;material: JLineDashedMaterial);overload;
    constructor Create(geometry: JBufferGeometry;material: JLineDashedMaterial;&type: integer);overload;
    constructor Create(geometry: JBufferGeometry;material: JLineBasicMaterial);overload;
    constructor Create(geometry: JBufferGeometry;material: JLineBasicMaterial;&type: integer);overload;
    constructor Create(geometry: JBufferGeometry;material: JShaderMaterial);overload;
    constructor Create(geometry: JBufferGeometry;material: JShaderMaterial;&type: integer);overload;
    function clone(object_: JLine): JLine;overload;
    property geometry: JGeometry;
    property material: JMaterial;
    property &type: JLineType;
  end;
  JLOD = class external(JObject3D)
  public
    constructor Create();
    procedure addLevel(object3D: JObject3D; visibleAtDistance: integer);overload;
    procedure update(camera: JCamera);
    function clone(): JLOD;
    property LODs: array of JObject3D;
  end;

  TMesh_morphTargetDictionary_object = class;

  JMesh = class external 'THREE.Mesh' (JObject3D)
  public
    constructor Create(geometry: JGeometry);overload;
    constructor Create(geometry: JGeometry;material: JMeshBasicMaterial);overload;
    constructor Create(geometry: JGeometry;material: JMeshDepthMaterial);overload;
    constructor Create(geometry: JGeometry;material: JMeshFaceMaterial);overload;
    constructor Create(geometry: JGeometry;material: JMeshLambertMaterial);overload;
    constructor Create(geometry: JGeometry;material: JMeshNormalMaterial);overload;
    constructor Create(geometry: JGeometry;material: JMeshPhongMaterial);overload;
    constructor Create(geometry: JGeometry;material: JShaderMaterial);overload;
    constructor Create(geometry: JBufferGeometry);overload;
    constructor Create(geometry: JBufferGeometry;material: JMeshBasicMaterial);overload;
    constructor Create(geometry: JBufferGeometry;material: JMeshDepthMaterial);overload;
    constructor Create(geometry: JBufferGeometry;material: JMeshFaceMaterial);overload;
    constructor Create(geometry: JBufferGeometry;material: JMeshLambertMaterial);overload;
    constructor Create(geometry: JBufferGeometry;material: JMeshNormalMaterial);overload;
    constructor Create(geometry: JBufferGeometry;material: JMeshPhongMaterial);overload;
    constructor Create(geometry: JBufferGeometry;material: JShaderMaterial);overload;
    procedure updateMorphTargets();
    function getMorphTargetIndexByName(name: string): integer;
    function clone(object_: JMesh): JMesh;overload;
    property geometry: JGeometry;
    property material: JMaterial;
    property morphTargetBase: integer;
    property morphTargetForcedOrder: integer;
    property morphTargetInfluences: array of integer;
    property morphTargetDictionary: TMesh_morphTargetDictionary_object;
  end;
  TMesh_morphTargetDictionary_object = class external "Object"
  public
    function  GetItems(key: string): integer; external array;
    procedure SetItems(key: string; value: integer); external array;
    property Items[key: string]: integer read GetItems write SetItems; default;
  end;
  JMorphAnimMesh = class external(JMesh)
  public
    constructor Create(geometry: JGeometry);overload;
    constructor Create(geometry: JGeometry;material: JMeshBasicMaterial);overload;
    constructor Create(geometry: JGeometry;material: JMeshDepthMaterial);overload;
    constructor Create(geometry: JGeometry;material: JMeshFaceMaterial);overload;
    constructor Create(geometry: JGeometry;material: JMeshLambertMaterial);overload;
    constructor Create(geometry: JGeometry;material: JMeshNormalMaterial);overload;
    constructor Create(geometry: JGeometry;material: JMeshPhongMaterial);overload;
    constructor Create(geometry: JGeometry;material: JShaderMaterial);overload;
    procedure setFrameRange(start: integer; &end: integer);
    procedure setDirectionForward();
    procedure setDirectionBackward();
    procedure parseAnimations();
    procedure setAnimationLabel(label: string; start: integer; &end: integer);
    procedure playAnimation(label: string; fps: integer);
    procedure updateAnimation(delta: integer);
    function clone(object_: JMorphAnimMesh): JMorphAnimMesh;overload;
    property duration: integer;
    property mirroredLoop: Jboolean;
    property time: integer;
    property lastKeyframe: integer;
    property currentKeyframe: integer;
    property direction: integer;
    property directionBackwards: Jboolean;
  end;
  JParticle = class external(JObject3D)
  public
    constructor Create(material: JMaterial);
    function clone(object_: JParticle): JParticle;overload;
  end;
  JParticleSystem = class external(JObject3D)
  public
    constructor Create(geometry: JGeometry; material: JParticleBasicMaterial);overload;
    constructor Create(geometry: JGeometry; material: JParticleCanvasMaterial);overload;
    constructor Create(geometry: JGeometry; material: JParticleDOMMaterial);overload;
    constructor Create(geometry: JGeometry; material: JShaderMaterial);overload;
    constructor Create(geometry: JBufferGeometry; material: JParticleBasicMaterial);overload;
    constructor Create(geometry: JBufferGeometry; material: JParticleCanvasMaterial);overload;
    constructor Create(geometry: JBufferGeometry; material: JParticleDOMMaterial);overload;
    constructor Create(geometry: JBufferGeometry; material: JShaderMaterial);overload;
    function clone(object_: JParticleSystem): JParticleSystem;overload;
    property geometry: JGeometry;
    property material: JMaterial;
    property sortParticles: Jboolean;
  end;
  JRibbon = class external(JObject3D)
  public
    constructor Create(geometry: JGeometry; material: JMaterial);
    function clone(object_: JRibbon): JRibbon;overload;
    property geometry: JGeometry;
    property material: JMaterial;
  end;
  JSkinnedMesh = class external(JMesh)
  public
    constructor Create(geometry: JGeometry);overload;
    constructor Create(geometry: JGeometry;material: JMeshBasicMaterial);overload;
    constructor Create(geometry: JGeometry;material: JMeshBasicMaterial;useVertexTexture: Jboolean);overload;
    constructor Create(geometry: JGeometry;material: JMeshDepthMaterial);overload;
    constructor Create(geometry: JGeometry;material: JMeshDepthMaterial;useVertexTexture: Jboolean);overload;
    constructor Create(geometry: JGeometry;material: JMeshFaceMaterial);overload;
    constructor Create(geometry: JGeometry;material: JMeshFaceMaterial;useVertexTexture: Jboolean);overload;
    constructor Create(geometry: JGeometry;material: JMeshLambertMaterial);overload;
    constructor Create(geometry: JGeometry;material: JMeshLambertMaterial;useVertexTexture: Jboolean);overload;
    constructor Create(geometry: JGeometry;material: JMeshNormalMaterial);overload;
    constructor Create(geometry: JGeometry;material: JMeshNormalMaterial;useVertexTexture: Jboolean);overload;
    constructor Create(geometry: JGeometry;material: JMeshPhongMaterial);overload;
    constructor Create(geometry: JGeometry;material: JMeshPhongMaterial;useVertexTexture: Jboolean);overload;
    constructor Create(geometry: JGeometry;material: JShaderMaterial);overload;
    constructor Create(geometry: JGeometry;material: JShaderMaterial;useVertexTexture: Jboolean);overload;
    function addBone(bone: JBone): JBone;overload;
    procedure updateMatrixWorld(force: Jboolean);overload;
    procedure pose();
    function clone(object_: JSkinnedMesh): JSkinnedMesh;overload;
    property useVertexTexture: Jboolean;
    property identityMatrix: JMatrix4;
    property bones: array of JBone;
    property boneMatrices: JFloat32Array;
    property boneTextureWidth: integer;
    property boneTextureHeight: integer;
    property boneTexture: JDataTexture;
    property {static?} offsetMatrix: JMatrix4;
  end;
  JSpriteParameters = class external "Object"
    property color: JColor;
    property map: JTexture;
    property blending: JBlending;
    property blendSrc: JBlendingSrcFactor;
    property blendDst: JBlendingDstFactor;
    property blendEquation: JBlendingEquation;
    property useScreenCoordinates: Jboolean;
    property mergeWith3D: Jboolean;
    property affectedByDistance: Jboolean;
    property alignment: JVector2;
    property fog: Jboolean;
    property uvOffset: JVector2;
    property uvScale: JVector2;
    property depthTest: Jboolean;
    property sizeAttenuation: Jboolean;
    property scaleByViewport: Jboolean;
  end;
  JSpriteMaterial = class external(JMaterial)
  public
    constructor Create(parameters: JSpriteParameters);overload;
    function clone(): JSpriteMaterial;
    property color: JColor;
    property map: JTexture;
    property blending: JBlending;
    property blendEquation: JBlendingEquation;
    property useScreenCoordinates: Jboolean;
    property scaleByViewport: Jboolean;
    property alignment: JVector2;
    property fog: Jboolean;
    property uvOffset: JVector2;
    property uvScale: JVector2;
    property depthTest: Jboolean;
    property sizeAttenuation: Jboolean;
  end;
  JSprite = class external(JObject3D)
  public
    constructor Create(material: JSpriteMaterial);overload;
    procedure updateMatrix();
    function clone(object_: JSprite): JSprite;overload;
  end;
  JSpriteAlignment = class external "Object"
    property {static?} topLeft: JVector2;
    property {static?} topCenter: JVector2;
    property {static?} topRight: JVector2;
    property {static?} centerLeft: JVector2;
    property {static?} center: JVector2;
    property {static?} centerRight: JVector2;
    property {static?} bottomLeft: JVector2;
    property {static?} bottomCenter: JVector2;
    property {static?} bottomRight: JVector2;
  end;
  JRenderer = class external "Object"
  public
    procedure render(scene: JScene; camera: JCamera);
  end;
  JCanvasRendererParameters = class external "Object"
    property canvas: JHTMLCanvasElement;
    property devicePixelRatio: integer;
  end;

  TCanvasRenderer_info_object = class;
  TCanvasRenderer_info_object_render_object = class;

  JCanvasRenderer = class external(JRenderer)
  public
    constructor Create(parameters: JCanvasRendererParameters);overload;
    procedure setSize(width: integer; height: integer);
    procedure setClearColor(color: JColor; opacity: integer);overload;
    procedure setClearColorHex(hex: integer; opacity: integer);overload;
    function getMaxAnisotropy(): integer;
    procedure clear();
    procedure render(scene: JScene; camera: JCamera);
    property domElement: JHTMLCanvasElement;
    property autoClear: Jboolean;
    property sortObjects: Jboolean;
    property sortElements: Jboolean;
    property devicePixelRatio: integer;
    property info: TCanvasRenderer_info_object;
  end;
  TCanvasRenderer_info_object = class external "Object"
    property render: TCanvasRenderer_info_object_render_object;
  end;
  TCanvasRenderer_info_object_render_object = class external "Object"
    property vertices: integer;
    property faces: integer;
  end;
  JShaderChunk = class external "Object"
  public
    function  GetItems(name: string): string; external array;
    procedure SetItems(name: string; value: string); external array;
    property Items[name: string]: string read GetItems write SetItems; default;
    property fog_pars_fragment: string;
    property fog_fragment: string;
    property envmap_pars_fragment: string;
    property envmap_fragment: string;
    property envmap_pars_vertex: string;
    property worldpos_vertex: string;
    property envmap_vertex: string;
    property map_particle_pars_fragment: string;
    property map_particle_fragment: string;
    property map_pars_vertex: string;
    property map_pars_fragment: string;
    property map_vertex: string;
    property map_fragment: string;
    property lightmap_pars_fragment: string;
    property lightmap_pars_vertex: string;
    property lightmap_fragment: string;
    property lightmap_vertex: string;
    property bumpmap_pars_fragment: string;
    property normalmap_pars_fragment: string;
    property specularmap_pars_fragment: string;
    property specularmap_fragment: string;
    property lights_lambert_pars_vertex: string;
    property lights_lambert_vertex: string;
    property lights_phong_pars_vertex: string;
    property lights_phong_vertex: string;
    property lights_phong_pars_fragment: string;
    property lights_phong_fragment: string;
    property color_pars_fragment: string;
    property color_fragment: string;
    property color_pars_vertex: string;
    property color_vertex: string;
    property skinning_pars_vertex: string;
    property skinbase_vertex: string;
    property skinning_vertex: string;
    property morphtarget_pars_vertex: string;
    property morphtarget_vertex: string;
    property default_vertex: string;
    property morphnormal_vertex: string;
    property skinnormal_vertex: string;
    property defaultnormal_vertex: string;
    property shadowmap_pars_fragment: string;
    property shadowmap_fragment: string;
    property shadowmap_pars_vertex: string;
    property shadowmap_vertex: string;
    property alphatest_fragment: string;
    property linear_to_gamma_fragment: string;
  end;
  JRendererPlugin = class external "Object"
  public
    procedure init(renderer: JWebGLRenderer);
    procedure render(scene: JScene; camera: JCamera; currentWidth: integer; currentHeight: integer);
  end;
  JWebGLRendererParameters = class external "Object"
    property canvas: JHTMLCanvasElement;
    property precision: string;
    property alpha: Jboolean;
    property premultipliedAlpha: Jboolean;
    property antialias: Jboolean;
    property stencil: Jboolean;
    property preserveDrawingBuffer: Jboolean;
    property clearColor: integer;
    property clearAlpha: integer;
    property devicePixelRatio: integer;
  end;

  TWebGLRenderer_info_object = class;
  TWebGLRenderer_info_object_memory_object = class;
  TWebGLRenderer_info_object_render_object = class;

  JWebGLRenderer = class external 'THREE.WebGLRenderer' (JRenderer)
  public
    constructor Create(parameters: JWebGLRendererParameters);overload;
    function getContext(): JWebGLRenderingContext;
    function supportsVertexTextures(): Jboolean;
    function getMaxAnisotropy(): integer;
    procedure setSize(width: integer; height: integer);
    procedure setViewport(x: integer);overload;
    procedure setViewport(x: integer;y: integer);overload;
    procedure setViewport(x: integer;y: integer;width: integer);overload;
    procedure setViewport(x: integer;y: integer;width: integer;height: integer);overload;
    procedure setScissor(x: integer; y: integer; width: integer; height: integer);
    procedure enableScissorTest(enable: Jboolean);
    procedure setClearColorHex(hex: integer; alpha: integer);
    procedure setClearColor(color: JColor; alpha: integer);
    function getClearColor(): JColor;
    function getClearAlpha(): integer;
    procedure clear(color: Jboolean);overload;
    procedure clear(color: Jboolean;depth: Jboolean);overload;
    procedure clear(color: Jboolean;depth: Jboolean;stencil: Jboolean);overload;
    procedure addPostPlugin(plugin: JRendererPlugin);
    procedure addPrePlugin(plugin: JRendererPlugin);
    procedure deallocateObject(object_: JObject3D);
    procedure deallocateTexture(texture: JTexture);
    procedure deallocateRenderTarget(renderTarget: JRenderTarget);
    procedure updateShadowMap(scene: JScene; camera: JCamera);
    procedure renderBufferImmediate(object_: JObject3D; program: JObject; material: JMaterial);
    procedure renderBufferDirect(camera: JCamera; lights: array of JLight; fog: JFog; material: JMaterial; geometryGroup: variant; object_: JObject3D);
    procedure renderBuffer(camera: JCamera; lights: array of JLight; fog: JFog; material: JMaterial; geometryGroup: variant; object_: JObject3D);
    procedure render(scene: JScene; camera: JCamera; renderTarget: JRenderTarget);overload;
    procedure render(scene: JScene; camera: JCamera; renderTarget: JRenderTarget;forceClear: Jboolean);overload;
    procedure renderImmediateObject(camera: JCamera; lights: array of JLight; fog: JFog; material: JMaterial; object_: JObject3D);
    procedure initWebGLObjects(scene: JScene);
    procedure initMaterial(material: JMaterial; lights: array of JLight; fog: JFog; object_: JObject3D);
    procedure setFaceCulling(cullFace: string);overload;
    procedure setFaceCulling(cullFace: string;frontFace: JFrontFaceDirection);overload;
    procedure setMaterialFaces(material: JMaterial);
    procedure setDepthTest(depthTest: Jboolean);
    procedure setDepthWrite(depthWrite: Jboolean);
    procedure setBlending(blending: JBlending; blendEquation: JBlendingEquation; blendSrc: JBlendingSrcFactor; blendDst: JBlendingDstFactor);
    procedure setTexture(texture: JTexture; slot: integer);
    procedure setRenderTarget(renderTarget: JRenderTarget);
    property domElement: JHTMLCanvasElement;
    property context: variant;
    property autoClear: Jboolean;
    property autoClearColor: Jboolean;
    property autoClearDepth: Jboolean;
    property autoClearStencil: Jboolean;
    property sortObjects: Jboolean;
    property autoUpdateObjects: Jboolean;
    property autoUpdateScene: Jboolean;
    property gammaInput: Jboolean;
    property gammaOutput: Jboolean;
    property physicallyBasedShading: Jboolean;
    property shadowMapEnabled: Jboolean;
    property shadowMapAutoUpdate: Jboolean;
    property shadowMapType: JShadowMapType;
    property shadowMapSoft: Jboolean;
    property shadowMapCullFace: JCullFace;
    property shadowMapDebug: Jboolean;
    property shadowMapCascade: Jboolean;
    property maxMorphTargets: integer;
    property maxMorphNormals: integer;
    property autoScaleCubemaps: Jboolean;
    property renderPluginsPre: array of JRendererPlugin;
    property renderPluginsPost: array of JRendererPlugin;
    property devicePixelRatio: integer;
    property info: TWebGLRenderer_info_object;
  end;
  TWebGLRenderer_info_object = class external "Object"
    property memory: TWebGLRenderer_info_object_memory_object;
    property render: TWebGLRenderer_info_object_render_object;
  end;
  TWebGLRenderer_info_object_memory_object = class external "Object"
    property programs: integer;
    property geometries: integer;
    property textures: integer;
  end;
  TWebGLRenderer_info_object_render_object = class external "Object"
    property calls: integer;
    property vertices: integer;
    property faces: integer;
    property points: integer;
  end;

  TWebGLRenderer2_info_object = class;
  TWebGLRenderer2_info_object_memory_object = class;
  TWebGLRenderer2_info_object_render_object = class;

  JWebGLRenderer2 = class external(JRenderer)
  public
    constructor Create(parameters: JWebGLRendererParameters);overload;
    function getContext(): JWebGLRenderingContext;
    function supportsVertexTextures(): Jboolean;
    function getMaxAnisotropy(): integer;
    procedure setSize(width: integer; height: integer);
    procedure setViewport(x: integer);overload;
    procedure setViewport(x: integer;y: integer);overload;
    procedure setViewport(x: integer;y: integer;width: integer);overload;
    procedure setViewport(x: integer;y: integer;width: integer;height: integer);overload;
    procedure setScissor(x: integer; y: integer; width: integer; height: integer);
    procedure enableScissorTest(enable: Jboolean);
    procedure setClearColorHex(hex: integer; alpha: integer);
    procedure setClearColor(color: JColor; alpha: integer);
    function getClearColor(): JColor;
    function getClearAlpha(): integer;
    procedure clear(color: Jboolean);overload;
    procedure clear(color: Jboolean;depth: Jboolean);overload;
    procedure clear(color: Jboolean;depth: Jboolean;stencil: Jboolean);overload;
    procedure addPostPlugin(plugin: JRendererPlugin);
    procedure addPrePlugin(plugin: JRendererPlugin);
    procedure deallocateObject(object_: JObject3D);
    procedure deallocateTexture(texture: JTexture);
    procedure deallocateRenderTarget(renderTarget: JRenderTarget);
    procedure updateShadowMap(scene: JScene; camera: JCamera);
    procedure renderBufferImmediate(object_: JObject3D; program: JObject; material: JMaterial);
    procedure renderBufferDirect(camera: JCamera; lights: array of JLight; fog: JFog; material: JMaterial; geometryGroup: variant; object_: JObject3D);
    procedure renderBuffer(camera: JCamera; lights: array of JLight; fog: JFog; material: JMaterial; geometryGroup: variant; object_: JObject3D);
    procedure render(scene: JScene; camera: JCamera; renderTarget: JRenderTarget);overload;
    procedure render(scene: JScene; camera: JCamera; renderTarget: JRenderTarget;forceClear: Jboolean);overload;
    procedure renderImmediateObject(camera: JCamera; lights: array of JLight; fog: JFog; material: JMaterial; object_: JObject3D);
    procedure initWebGLObjects(scene: JScene);
    procedure initMaterial(material: JMaterial; lights: array of JLight; fog: JFog; object_: JObject3D);
    procedure setFaceCulling(cullFace: string);overload;
    procedure setFaceCulling(cullFace: string;frontFace: JFrontFaceDirection);overload;
    procedure setMaterialFaces(material: JMaterial);
    procedure setDepthTest(depthTest: Jboolean);
    procedure setDepthWrite(depthWrite: Jboolean);
    procedure setBlending(blending: JBlending; blendEquation: JBlendingEquation; blendSrc: JBlendingSrcFactor; blendDst: JBlendingDstFactor);
    procedure setTexture(texture: JTexture; slot: integer);
    procedure setRenderTarget(renderTarget: JRenderTarget);
    property domElement: JHTMLCanvasElement;
    property context: JWebGLRenderingContext;
    property autoClear: Jboolean;
    property autoClearColor: Jboolean;
    property autoClearDepth: Jboolean;
    property autoClearStencil: Jboolean;
    property sortObjects: Jboolean;
    property autoUpdateObjects: Jboolean;
    property autoUpdateScene: Jboolean;
    property gammaInput: Jboolean;
    property gammaOutput: Jboolean;
    property physicallyBasedShading: Jboolean;
    property shadowMapEnabled: Jboolean;
    property shadowMapAutoUpdate: Jboolean;
    property shadowMapType: JShadowMapType;
    property shadowMapSoft: Jboolean;
    property shadowMapCullFace: JCullFace;
    property shadowMapDebug: Jboolean;
    property shadowMapCascade: Jboolean;
    property maxMorphTargets: integer;
    property maxMorphNormals: integer;
    property autoScaleCubemaps: Jboolean;
    property renderPluginsPre: array of JRendererPlugin;
    property renderPluginsPost: array of JRendererPlugin;
    property devicePixelRatio: integer;
    property info: TWebGLRenderer2_info_object;
  end;
  TWebGLRenderer2_info_object = class external "Object"
    property memory: TWebGLRenderer2_info_object_memory_object;
    property render: TWebGLRenderer2_info_object_render_object;
  end;
  TWebGLRenderer2_info_object_memory_object = class external "Object"
    property programs: integer;
    property geometries: integer;
    property textures: integer;
  end;
  TWebGLRenderer2_info_object_render_object = class external "Object"
    property calls: integer;
    property vertices: integer;
    property faces: integer;
    property points: integer;
  end;
  JRenderTarget = class external "Object"
  end;
  JWebGLRenderTargetOptions = class external "Object"
    property wrapS: JWrapping;
    property wrapT: JWrapping;
    property magFilter: JTextureFilter;
    property minFilter: JTextureFilter;
    property anisotropy: integer;
    property format: integer;
    property &type: JTextureDataType;
    property depthBuffer: Jboolean;
    property stencilBuffer: Jboolean;
  end;
  JWebGLRenderTarget = class external(JRenderTarget)
  public
    constructor Create(width: integer; height: integer; options: JWebGLRenderTargetOptions);overload;
    function clone(): JWebGLRenderTarget;
    procedure dispose();
    property width: integer;
    property height: integer;
    property wrapS: JWrapping;
    property wrapT: JWrapping;
    property magFilter: JTextureFilter;
    property minFilter: JTextureFilter;
    property anisotropy: integer;
    property offset: JVector2;
    property &repeat: JVector2;
    property format: integer;
    property &type: integer;
    property depthBuffer: Jboolean;
    property stencilBuffer: Jboolean;
    property generateMipmaps: Jboolean;
  end;
  JWebGLRenderTargetCube = class external(JWebGLRenderTarget)
  public
    constructor Create(width: integer; height: integer; options: JWebGLRenderTargetOptions);overload;
    property activeCubeFace: integer;
  end;
  JRenderableFace3 = class external "Object"
  public
    constructor Create();
    property v1: JRenderableVertex;
    property v2: JRenderableVertex;
    property v3: JRenderableVertex;
    property centroidWorld: JVector3;
    property centroidScreen: JVector3;
    property normalWorld: JVector3;
    property vertexNormalsWorld: array of JVector3;
    property vertexNormalsLength: integer;
    property color: integer;
    property material: JMaterial;
    property uvs: array of array of JVector2;
    property z: integer;
  end;
  JRenderableFace4 = class external "Object"
  public
    constructor Create();
    property v1: JRenderableVertex;
    property v2: JRenderableVertex;
    property v3: JRenderableVertex;
    property v4: JRenderableVertex;
    property centroidWorld: JVector3;
    property centroidScreen: JVector3;
    property normalWorld: JVector3;
    property vertexNormalsWorld: array of JVector3;
    property vertexNormalsLength: integer;
    property color: integer;
    property material: JMaterial;
    property uvs: array of array of JVector2;
    property z: integer;
  end;
  JRenderableLine = class external "Object"
  public
    constructor Create();
    property z: integer;
    property v1: JRenderableVertex;
    property v2: JRenderableVertex;
    property material: JMaterial;
  end;
  JRenderableObject = class external "Object"
  public
    constructor Create();
    property &object: JObject;
    property z: integer;
  end;
  JRenderableParticle = class external "Object"
  public
    constructor Create();
    property &object: JObject;
    property x: integer;
    property y: integer;
    property z: integer;
    property rotation: integer;
    property scale: JVector2;
    property material: JMaterial;
  end;
  JRenderableVertex = class external "Object"
  public
    constructor Create();
    procedure copy(vertex: JRenderableVertex);
    property positionWorld: JVector3;
    property positionScreen: JVector4;
    property visible: Jboolean;
  end;
  JIFog = class external "Object"
  public
    function clone(): JIFog;
    property name: string;
    property color: JColor;
  end;
  JFog = class external(JIFog)
  public
    constructor Create(hex: integer; near: integer);overload;
    constructor Create(hex: integer; near: integer;far: integer);overload;
    function clone(): JFog;
    property name: string;
    property color: JColor;
    property near: integer;
    property far: integer;
  end;
  JFogExp2 = class external(JIFog)
  public
    constructor Create(hex: integer; density: integer);overload;
    function clone(): JFogExp2;
    property name: string;
    property color: JColor;
    property density: integer;
  end;
  JScene = class external 'THREE.Scene' (JObject3D)
  public
    constructor Create();
    property fog: JIFog;
    property overrideMaterial: JMaterial;
    property matrixAutoUpdate: Jboolean;
  end;

  TTexture_onUpdate_ = procedure ();

  JTexture = class external "Object"
  public
    constructor Create(image: JHTMLImageElement; mapping: JMapping);overload;
    constructor Create(image: JHTMLImageElement; mapping: JMapping;wrapS: JWrapping);overload;
    constructor Create(image: JHTMLImageElement; mapping: JMapping;wrapS: JWrapping;wrapT: JWrapping);overload;
    constructor Create(image: JHTMLImageElement; mapping: JMapping;wrapS: JWrapping;wrapT: JWrapping;magFilter: JTextureFilter);overload;
    constructor Create(image: JHTMLImageElement; mapping: JMapping;wrapS: JWrapping;wrapT: JWrapping;magFilter: JTextureFilter;minFilter: JTextureFilter);overload;
    constructor Create(image: JHTMLImageElement; mapping: JMapping;wrapS: JWrapping;wrapT: JWrapping;magFilter: JTextureFilter;minFilter: JTextureFilter;format: JPixelFormat);overload;
    constructor Create(image: JHTMLImageElement; mapping: JMapping;wrapS: JWrapping;wrapT: JWrapping;magFilter: JTextureFilter;minFilter: JTextureFilter;format: JPixelFormat;&type: JTextureDataType);overload;
    constructor Create(image: JHTMLImageElement; mapping: JMapping;wrapS: JWrapping;wrapT: JWrapping;magFilter: JTextureFilter;minFilter: JTextureFilter;format: JPixelFormat;&type: JTextureDataType;anisotropy: integer);overload;
    constructor Create(image: JHTMLCanvasElement; mapping: JMapping);overload;
    constructor Create(image: JHTMLCanvasElement; mapping: JMapping;wrapS: JWrapping);overload;
    constructor Create(image: JHTMLCanvasElement; mapping: JMapping;wrapS: JWrapping;wrapT: JWrapping);overload;
    constructor Create(image: JHTMLCanvasElement; mapping: JMapping;wrapS: JWrapping;wrapT: JWrapping;magFilter: JTextureFilter);overload;
    constructor Create(image: JHTMLCanvasElement; mapping: JMapping;wrapS: JWrapping;wrapT: JWrapping;magFilter: JTextureFilter;minFilter: JTextureFilter);overload;
    constructor Create(image: JHTMLCanvasElement; mapping: JMapping;wrapS: JWrapping;wrapT: JWrapping;magFilter: JTextureFilter;minFilter: JTextureFilter;format: JPixelFormat);overload;
    constructor Create(image: JHTMLCanvasElement; mapping: JMapping;wrapS: JWrapping;wrapT: JWrapping;magFilter: JTextureFilter;minFilter: JTextureFilter;format: JPixelFormat;&type: JTextureDataType);overload;
    constructor Create(image: JHTMLCanvasElement; mapping: JMapping;wrapS: JWrapping;wrapT: JWrapping;magFilter: JTextureFilter;minFilter: JTextureFilter;format: JPixelFormat;&type: JTextureDataType;anisotropy: integer);overload;
    function clone(): JTexture;
    procedure dispose();
    property id: integer;
    property name: string;
    property image: JObject;
    property mapping: JMapping;
    property wrapS: JWrapping;
    property wrapT: JWrapping;
    property magFilter: JTextureFilter;
    property minFilter: JTextureFilter;
    property anisotropy: integer;
    property format: JPixelFormat;
    property &type: JTextureDataType;
    property offset: JVector2;
    property &repeat: JVector2;
    property generateMipmaps: Jboolean;
    property premultiplyAlpha: Jboolean;
    property flipY: Jboolean;
    property needsUpdate: Jboolean;
    property onUpdate: TTexture_onUpdate_;
  end;
  JCompressedTexture = class external(JTexture)
  public
    constructor Create(mipmaps: array of JImageData; width: integer; height: integer; format: JPixelFormat);overload;
    constructor Create(mipmaps: array of JImageData; width: integer; height: integer; format: JPixelFormat;&type: JTextureDataType);overload;
    constructor Create(mipmaps: array of JImageData; width: integer; height: integer; format: JPixelFormat;&type: JTextureDataType;mapping: JMapping);overload;
    constructor Create(mipmaps: array of JImageData; width: integer; height: integer; format: JPixelFormat;&type: JTextureDataType;mapping: JMapping;wrapS: JWrapping);overload;
    constructor Create(mipmaps: array of JImageData; width: integer; height: integer; format: JPixelFormat;&type: JTextureDataType;mapping: JMapping;wrapS: JWrapping;wrapT: JWrapping);overload;
    constructor Create(mipmaps: array of JImageData; width: integer; height: integer; format: JPixelFormat;&type: JTextureDataType;mapping: JMapping;wrapS: JWrapping;wrapT: JWrapping;magFilter: JTextureFilter);overload;
    constructor Create(mipmaps: array of JImageData; width: integer; height: integer; format: JPixelFormat;&type: JTextureDataType;mapping: JMapping;wrapS: JWrapping;wrapT: JWrapping;magFilter: JTextureFilter;minFilter: JTextureFilter);overload;
    function clone(): JCompressedTexture;
    property mipmaps: array of JImageData;
  end;
  JDataTexture = class external(JTexture)
  public
    constructor Create(data: JImageData; width: integer; height: integer; format: JPixelFormat; &type: JTextureDataType; mapping: JMapping; wrapS: JWrapping; wrapT: JWrapping; magFilter: JTextureFilter; minFilter: JTextureFilter);
    function clone(): JDataTexture;
  end;

  TFontUtils_JFontUtils_faces_object = class;
  TFontUtils_JFontUtils_faces_object_Items_result_object = class;
  TFontUtils_JFontUtils_drawText_result_object = class;
  TFontUtils_JFontUtils_extractGlyphPoints_result_object = class;
  TFontUtils_JFontUtils_generateShapes_parameters_object = class;
  TFontUtils_JFontUtils_Triangulate_object = class;
  TImageUtils_JImageUtils_loadTexture_onLoad_ = procedure (texture: JTexture);
  TImageUtils_JImageUtils_loadTexture_onError_ = procedure (message: string);
  TImageUtils_JImageUtils_loadCompressedTexture_onLoad_ = procedure (texture: JTexture);
  TImageUtils_JImageUtils_loadCompressedTexture_onError_ = procedure (message: string);
  TImageUtils_JImageUtils_loadTextureCube_onLoad_ = procedure ();
  TImageUtils_JImageUtils_loadTextureCube_onError_ = procedure (message: string);
  TImageUtils_JImageUtils_loadCompressedTextureCube_onLoad_ = procedure ();
  TImageUtils_JImageUtils_loadCompressedTextureCube_onError_ = procedure (message: string);
  TImageUtils_JImageUtils_parseDDS_result_object = class;
  TImageUtils_JImageUtils_parseDDS_result_object_mipmaps_object = class;

  JTypefaceData = class external "Object"
    property familyName: string;
    property cssFontWeight: string;
    property cssFontStyle: string;
  end;
  JFontUtils = class external "FontUtils"
  public
    function getFace(): JFace;
    function loadFace(data: JTypefaceData): JTypefaceData;
    function drawText(text: string): TFontUtils_JFontUtils_drawText_result_object;
    function extractGlyphPoints(c: string; face: JFace; scale: integer; offset: integer; path: JPath): TFontUtils_JFontUtils_extractGlyphPoints_result_object;
    function generateShapes(text: string; parameters: TFontUtils_JFontUtils_generateShapes_parameters_object): array of JShape;overload;
    property faces: TFontUtils_JFontUtils_faces_object;
    property face: string;
    property weight: string;
    property style: string;
    property size: integer;
    property divisions: integer;
    property Triangulate: TFontUtils_JFontUtils_Triangulate_object;
  end;
  TFontUtils_JFontUtils_faces_object = class external "Object"
  public
    function  GetItems(weight: string): TFontUtils_JFontUtils_faces_object_Items_result_object; external array;
    procedure SetItems(weight: string; value: TFontUtils_JFontUtils_faces_object_Items_result_object); external array;
    property Items[weight: string]: TFontUtils_JFontUtils_faces_object_Items_result_object read GetItems write SetItems; default;
  end;
  TFontUtils_JFontUtils_faces_object_Items_result_object = class external "Object"
  public
    function  GetItems(style: string): JFace; external array;
    procedure SetItems(style: string; value: JFace); external array;
    property Items[style: string]: JFace read GetItems write SetItems; default;
  end;
  TFontUtils_JFontUtils_drawText_result_object = class external "Object"
    property paths: array of JPath;
    property offset: integer;
  end;
  TFontUtils_JFontUtils_extractGlyphPoints_result_object = class external "Object"
    property offset: integer;
    property path: JPath;
  end;
  TFontUtils_JFontUtils_generateShapes_parameters_object = class external "Object"
    property size: integer;
    property curveSegments: integer;
    property font: string;
    property weight: string;
    property style: string;
  end;
  TFontUtils_JFontUtils_Triangulate_object = class external "Object"
  public
    function area(contour: array of JVector2): integer;
  end;
  JGeometryUtils = class external "GeometryUtils"
  public
    procedure merge(geometry1: JGeometry; object2: JMesh);overload;
    procedure merge(geometry1: JGeometry; object2: JGeometry);overload;
    procedure removeMaterials(geometry: JGeometry; materialIndexArray: array of integer);
    function randomPointInTriangle(vectorA: JVector3; vectorB: JVector3; vectorC: JVector3): JVector3;
    function randomPointInFace(face: JFace; geometry: JGeometry; useCachedAreas: Jboolean): JVector3;
    function randomPointsInGeometry(geometry: JGeometry; n: integer): JVector3;
    function triangleArea(vectorA: JVector3; vectorB: JVector3; vectorC: JVector3): integer;
    function center(geometry: JGeometry): JVector3;
    procedure normalizeUVs(geometry: JGeometry);
    procedure triangulateQuads(geometry: JGeometry);
    procedure setMaterialIndex(geometry: JGeometry; index: integer; startFace: integer);overload;
    procedure setMaterialIndex(geometry: JGeometry; index: integer; startFace: integer;endFace: integer);overload;
  end;
  JImageUtils = class external "ImageUtils"
  public
    function loadTexture(url: string; mapping: JMapping): JTexture;overload;
    function loadTexture(url: string; mapping: JMapping;onLoad: TImageUtils_JImageUtils_loadTexture_onLoad_): JTexture;overload;
    function loadTexture(url: string; mapping: JMapping;onLoad: TImageUtils_JImageUtils_loadTexture_onLoad_;onError: TImageUtils_JImageUtils_loadTexture_onError_): JTexture;overload;
    function loadCompressedTexture(url: string; mapping: JMapping): JTexture;overload;
    function loadCompressedTexture(url: string; mapping: JMapping;onLoad: TImageUtils_JImageUtils_loadCompressedTexture_onLoad_): JTexture;overload;
    function loadCompressedTexture(url: string; mapping: JMapping;onLoad: TImageUtils_JImageUtils_loadCompressedTexture_onLoad_;onError: TImageUtils_JImageUtils_loadCompressedTexture_onError_): JTexture;overload;
    function loadTextureCube(&array: array of string; mapping: JMapping): JTexture;overload;
    function loadTextureCube(&array: array of string; mapping: JMapping;onLoad: TImageUtils_JImageUtils_loadTextureCube_onLoad_): JTexture;overload;
    function loadTextureCube(&array: array of string; mapping: JMapping;onLoad: TImageUtils_JImageUtils_loadTextureCube_onLoad_;onError: TImageUtils_JImageUtils_loadTextureCube_onError_): JTexture;overload;
    function loadCompressedTextureCube(&array: array of string; mapping: JMapping): JTexture;overload;
    function loadCompressedTextureCube(&array: array of string; mapping: JMapping;onLoad: TImageUtils_JImageUtils_loadCompressedTextureCube_onLoad_): JTexture;overload;
    function loadCompressedTextureCube(&array: array of string; mapping: JMapping;onLoad: TImageUtils_JImageUtils_loadCompressedTextureCube_onLoad_;onError: TImageUtils_JImageUtils_loadCompressedTextureCube_onError_): JTexture;overload;
    function parseDDS(buffer: JArrayBuffer; loadMipmaps: Jboolean): TImageUtils_JImageUtils_parseDDS_result_object;
    function getNormalMap(image: JHTMLImageElement; depth: integer): JHTMLCanvasElement;overload;
    function generateDataTexture(width: integer; height: integer; color: JColor): JDataTexture;
    property crossOrigin: string;
  end;
  TImageUtils_JImageUtils_parseDDS_result_object = class external "Object"
    property mipmaps: array of TImageUtils_JImageUtils_parseDDS_result_object_mipmaps_object;
    property width: integer;
    property height: integer;
    property format: integer;
    property mipmapCount: integer;
  end;
  TImageUtils_JImageUtils_parseDDS_result_object_mipmaps_object = class external "Object"
    property data: JUint8Array;
    property width: integer;
    property height: integer;
  end;
  JSceneUtils = class external "SceneUtils"
  public
    function createMultiMaterialObject(geometry: JGeometry; materials: array of JMaterial): JObject3D;
    procedure detach(child: JObject3D; parent: JObject3D; scene: JScene);
    procedure attach(child: JObject3D; scene: JScene; parent: JObject3D);
  end;
  JKeyFrame = class external "Object"
    property pos: array of integer;
    property rot: array of integer;
    property scl: array of integer;
    property time: integer;
  end;
  JKeyFrames = class external "Object"
    property keys: array of JKeyFrame;
    property parent: integer;
  end;
  JAnimationData = class external "Object"
    property JIT: integer;
    property fps: integer;
    property hierarchy: array of JKeyFrames;
    property length: integer;
    property name: string;
  end;
  JAnimation = class external "Object"
  public
    constructor Create(root: JMesh; name: string; interpolationType: JAnimationInterpolation);overload;
    function interpolateCatmullRom(points: array of JVector3; scale: integer): array of JVector3;
    function interpolate(p0: integer; p1: integer; p2: integer; p3: integer; t: integer; t2: integer; t3: integer): integer;
    procedure play(loop: Jboolean);overload;
    procedure play(loop: Jboolean;startTimeMS: integer);overload;
    procedure pause();
    procedure stop();
    procedure update(deltaTimeMS: integer);
    function getNextKeyWith(&type: string; h: integer; key: integer): JKeyFrame;
    function getPrevKeyWith(&type: string; h: integer; key: integer): JKeyFrame;
    property root: JMesh;
    property data: JAnimationData;
    property hierarchy: array of JBone;
    property currentTime: integer;
    property timeScale: integer;
    property isPlaying: Jboolean;
    property isPaused: Jboolean;
    property loop: Jboolean;
    property interpolationType: JAnimationInterpolation;
    property points: array of JVector3;
    property target: JVector3;
    property JITCompile: Jboolean;
  end;
  JAnimationInterpolation = class external "Object"
  end;
  JAnimationHandler = class external "AnimationHandler"
  public
    procedure update(deltaTimeMS: integer);
    procedure addToUpdate(animation: JAnimation);
    procedure removeFromUpdate(animation: JAnimation);
    procedure add(data: JAnimationData);
    function get(name: string): JAnimationData;
    function parse(root: JSkinnedMesh): array of JObject3D;
    property LINEAR: JAnimationInterpolation;
    property CATMULLROM: JAnimationInterpolation;
    property CATMULLROM_FORWARD: JAnimationInterpolation;
  end;
  JAnimationMorphTarget = class external "Object"
  public
    constructor Create(root: JBone; data: variant);
    procedure play(loop: Jboolean);overload;
    procedure play(loop: Jboolean;startTimeMS: integer);overload;
    procedure pause();
    procedure stop();
    procedure update(deltaTimeMS: integer);
    property influence: integer;
    property root: JBone;
    property data: JObject;
    property hierarchy: array of JKeyFrames;
    property currentTime: integer;
    property timeScale: integer;
    property isPlaying: Jboolean;
    property isPaused: Jboolean;
    property loop: Jboolean;
  end;
  JKeyFrameAnimation = class external "Object"
  public
    constructor Create(root: JMesh; data: variant; JITCompile: Jboolean);overload;
    procedure play(loop: integer);overload;
    procedure play(loop: integer;startTimeMS: integer);overload;
    procedure pause();
    procedure stop();
    procedure update(deltaTimeMS: integer);
    function getNextKeyWith(&type: string; h: integer; key: integer): JKeyFrame;
    function getPrevKeyWith(&type: string; h: integer; key: integer): JKeyFrame;
    property JITCompile: integer;
    property root: JMesh;
    property data: JObject;
    property hierarchy: array of JKeyFrames;
    property currentTime: integer;
    property timeScale: integer;
    property isPlaying: integer;
    property isPaused: integer;
    property loop: integer;
  end;
  JCombinedCamera = class external(JCamera)
  public
    constructor Create(width: integer; height: integer; fov: integer; near: integer; far: integer; orthoNear: integer; orthoFar: integer);
    procedure toPerspective();
    procedure toOrthographic();
    procedure setSize(width: integer; height: integer);
    procedure setFov(fov: integer);
    procedure updateProjectionMatrix();
    function setLens(focalLength: integer; frameHeight: integer): integer;overload;
    procedure setZoom(zoom: integer);
    procedure toFrontView();
    procedure toBackView();
    procedure toLeftView();
    procedure toRightView();
    procedure toTopView();
    procedure toBottomView();
    property fov: integer;
    property left: integer;
    property right: integer;
    property top: integer;
    property bottom: integer;
    property cameraO: JOrthographicCamera;
    property cameraP: JPerspectiveCamera;
    property zoom: integer;
    property near: integer;
    property far: integer;
    property inPerspectiveMode: Jboolean;
    property inOrthographicMode: Jboolean;
  end;
  JCubeCamera = class external(JObject3D)
  public
    constructor Create(near: integer; far: integer; cubeResolution: integer);
    procedure updateCubeMap(renderer: JRenderer; scene: JScene);
    property renderTarget: JWebGLRenderTargetCube;
  end;

  TCurve_Utils_object = class;

  JCurve = class external "Object"
  public
    constructor Create();
    function getPoint(t: integer): JVector;
    function getPointAt(u: integer): JVector;
    function getPoints(divisions: integer): array of JVector;overload;
    function getSpacedPoints(divisions: integer): array of JVector;overload;
    function getLength(): integer;
    function getLengths(divisions: integer): array of integer;overload;
    procedure updateArcLengths();
    function getUtoTmapping(u: integer; distance: integer): integer;
    function getNormalVector(t: integer): JVector;
    function getTangent(t: integer): JVector;
    function getTangentAt(u: integer): JVector;
    //function create(constructorFunc: JFunction; getPointFunc: JFunction): JFunction;
    property needsUpdate: Jboolean;
    property {static?} Utils: TCurve_Utils_object;
  end;
  TCurve_Utils_object = class external "Object"
  public
    function tangentQuadraticBezier(t: integer; p0: integer; p1: integer; p2: integer): integer;
    function tangentCubicBezier(t: integer; p0: integer; p1: integer; p2: integer; p3: integer): integer;
    function tangentSpline(t: integer; p0: integer; p1: integer; p2: integer; p3: integer): integer;
    function interpolate(p0: integer; p1: integer; p2: integer; p3: integer; t: integer): integer;
  end;
  JLineCurve = class external(JCurve)
  public
    constructor Create(v1: JVector2; v2: JVector2);
    function getPoint(t: integer): JVector2;
    function getPointAt(u: integer): JVector2;
    function getPoints(divisions: integer): array of JVector2;overload;
    function getSpacedPoints(divisions: integer): array of JVector2;overload;
    function getLength(): integer;
    function getLengths(divisions: integer): array of integer;overload;
    procedure updateArcLengths();
    function getUtoTmapping(u: integer; distance: integer): integer;
    function getNormalVector(t: integer): JVector2;
    function getTangent(t: integer): JVector2;
    function getTangentAt(u: integer): JVector2;
    property needsUpdate: Jboolean;
  end;
  JQuadraticBezierCurve = class external(JCurve)
  public
    constructor Create(v0: JVector2; v1: JVector2; v2: JVector2);
    function getPoint(t: integer): JVector2;
    function getPointAt(u: integer): JVector2;
    function getPoints(divisions: integer): array of JVector2;overload;
    function getSpacedPoints(divisions: integer): array of JVector2;overload;
    function getLength(): integer;
    function getLengths(divisions: integer): array of integer;overload;
    procedure updateArcLengths();
    function getUtoTmapping(u: integer; distance: integer): integer;
    function getNormalVector(t: integer): JVector2;
    function getTangent(t: integer): JVector2;
    function getTangentAt(u: integer): JVector2;
    property needsUpdate: Jboolean;
  end;
  JCubicBezierCurve = class external(JCurve)
  public
    constructor Create(v0: integer; v1: integer; v2: integer; v3: integer);
    function getPoint(t: integer): JVector2;
    function getPointAt(u: integer): JVector2;
    function getPoints(divisions: integer): array of JVector2;overload;
    function getSpacedPoints(divisions: integer): array of JVector2;overload;
    function getLength(): integer;
    function getLengths(divisions: integer): array of integer;overload;
    procedure updateArcLengths();
    function getUtoTmapping(u: integer; distance: integer): integer;
    function getNormalVector(t: integer): JVector2;
    function getTangent(t: integer): JVector2;
    function getTangentAt(u: integer): JVector2;
    property needsUpdate: Jboolean;
  end;
  JSplineCurve = class external(JCurve)
  public
    constructor Create(points: array of JVector2);overload;
    function getPoint(t: integer): JVector2;
    function getPointAt(u: integer): JVector2;
    function getPoints(divisions: integer): array of JVector2;overload;
    function getSpacedPoints(divisions: integer): array of JVector2;overload;
    function getLength(): integer;
    function getLengths(divisions: integer): array of integer;overload;
    procedure updateArcLengths();
    function getUtoTmapping(u: integer; distance: integer): integer;
    function getNormalVector(t: integer): JVector2;
    function getTangent(t: integer): JVector2;
    function getTangentAt(u: integer): JVector2;
    property needsUpdate: Jboolean;
  end;
  JEllipseCurve = class external(JCurve)
  public
    constructor Create(aX: integer; aY: integer; xRadius: integer; yRadius: integer; aStartAngle: integer; aEndAngle: integer; aClockwise: Jboolean);
    function getPoint(t: integer): JVector2;
    function getPointAt(u: integer): JVector2;
    function getPoints(divisions: integer): array of JVector2;overload;
    function getSpacedPoints(divisions: integer): array of JVector2;overload;
    function getLength(): integer;
    function getLengths(divisions: integer): array of integer;overload;
    procedure updateArcLengths();
    function getUtoTmapping(u: integer; distance: integer): integer;
    function getNormalVector(t: integer): JVector2;
    function getTangent(t: integer): JVector2;
    function getTangentAt(u: integer): JVector2;
    property aX: integer;
    property aY: integer;
    property xRadius: integer;
    property yRadius: integer;
    property aStartAngle: integer;
    property aEndAngle: integer;
    property aClockwise: Jboolean;
    property needsUpdate: Jboolean;
  end;
  JArcCurve = class external(JEllipseCurve)
  public
    constructor Create(aX: integer; aY: integer; aRadius: integer; aStartAngle: integer; aEndAngle: integer; aClockwise: Jboolean);
    function getPoint(t: integer): JVector2;
    function getPointAt(u: integer): JVector2;
    function getPoints(divisions: integer): array of JVector2;overload;
    function getSpacedPoints(divisions: integer): array of JVector2;overload;
    function getLength(): integer;
    function getLengths(divisions: integer): array of integer;overload;
    procedure updateArcLengths();
    function getUtoTmapping(u: integer; distance: integer): integer;
    function getNormalVector(t: integer): JVector2;
    function getTangent(t: integer): JVector2;
    function getTangentAt(u: integer): JVector2;
    property needsUpdate: Jboolean;
  end;
  JLineCurve3 = class external(JCurve)
  public
    constructor Create(v1: JVector3; v2: JVector3);
    function getPoint(t: integer): JVector3;
    function getPointAt(u: integer): JVector3;
    function getPoints(divisions: integer): array of JVector3;overload;
    function getSpacedPoints(divisions: integer): array of JVector3;overload;
    function getLength(): integer;
    function getLengths(divisions: integer): array of integer;overload;
    procedure updateArcLengths();
    function getUtoTmapping(u: integer; distance: integer): integer;
    function getNormalVector(t: integer): JVector3;
    function getTangent(t: integer): JVector3;
    function getTangentAt(u: integer): JVector3;
    property needsUpdate: Jboolean;
  end;
  JQuadraticBezierCurve3 = class external(JCurve)
  public
    constructor Create(v0: JVector3; v1: JVector3; v2: JVector3);
    function getPoint(t: integer): JVector3;
    function getPointAt(u: integer): JVector3;
    function getPoints(divisions: integer): array of JVector3;overload;
    function getSpacedPoints(divisions: integer): array of JVector3;overload;
    function getLength(): integer;
    function getLengths(divisions: integer): array of integer;overload;
    procedure updateArcLengths();
    function getUtoTmapping(u: integer; distance: integer): integer;
    function getNormalVector(t: integer): JVector3;
    function getTangent(t: integer): JVector3;
    function getTangentAt(u: integer): JVector3;
    property needsUpdate: Jboolean;
  end;
  JCubicBezierCurve3 = class external(JCurve)
  public
    constructor Create(v0: JVector3; v1: JVector3; v2: JVector3; v3: JVector3);
    function getPoint(t: integer): JVector3;
    function getPointAt(u: integer): JVector3;
    function getPoints(divisions: integer): array of JVector3;overload;
    function getSpacedPoints(divisions: integer): array of JVector3;overload;
    function getLength(): integer;
    function getLengths(divisions: integer): array of integer;overload;
    procedure updateArcLengths();
    function getUtoTmapping(u: integer; distance: integer): integer;
    function getNormalVector(t: integer): JVector3;
    function getTangent(t: integer): JVector3;
    function getTangentAt(u: integer): JVector3;
    property needsUpdate: Jboolean;
  end;
  JSplineCurve3 = class external(JCurve)
  public
    constructor Create(points: array of JVector3);overload;
    function getPoint(t: integer): JVector3;
    function getPointAt(u: integer): JVector3;
    function getPoints(divisions: integer): array of JVector3;overload;
    function getSpacedPoints(divisions: integer): array of JVector3;overload;
    function getLength(): integer;
    function getLengths(divisions: integer): array of integer;overload;
    procedure updateArcLengths();
    function getUtoTmapping(u: integer; distance: integer): integer;
    function getNormalVector(t: integer): JVector3;
    function getTangent(t: integer): JVector3;
    function getTangentAt(u: integer): JVector3;
    property points: array of JVector3;
    property needsUpdate: Jboolean;
  end;
  JClosedSplineCurve3 = class external(JCurve)
  public
    constructor Create(points: array of JVector3);overload;
    function getPoint(t: integer): JVector3;
    function getPointAt(u: integer): JVector3;
    function getPoints(divisions: integer): array of JVector3;overload;
    function getSpacedPoints(divisions: integer): array of JVector3;overload;
    function getLength(): integer;
    function getLengths(divisions: integer): array of integer;overload;
    procedure updateArcLengths();
    function getUtoTmapping(u: integer; distance: integer): integer;
    function getNormalVector(t: integer): JVector3;
    function getTangent(t: integer): JVector3;
    function getTangentAt(u: integer): JVector3;
    property points: array of JVector3;
    property needsUpdate: Jboolean;
  end;
  JBoundingBox = class external "Object"
    property minX: integer;
    property minY: integer;
    property maxX: integer;
    property maxY: integer;
    property centroid: JVector;
  end;

TPathActions = (MOVE_TO,LINE_TO,QUADRATIC_CURVE_TO,BEZIER_CURVE_TO,CSPLINE_THRU,ARC,ELLIPSE);

  JCurvePath = class external(JCurve)
  public
    constructor Create();
    procedure add(curve: JCurve);
    function checkConnection(): Jboolean;
    procedure closePath();
    function getBoundingBox(): JBoundingBox;
    function createPointsGeometry(divisions: integer): JGeometry;
    function createSpacedPointsGeometry(divisions: integer): JGeometry;
    function createGeometry(points: array of JVector2): JGeometry;
    procedure addWrapPath(bendpath: JPath);
    function getTransformedPoints(segments: integer; bends: JPath): array of JVector2;overload;
    function getTransformedSpacedPoints(segments: integer; bends: array of JPath): array of JVector2;overload;
    function getWrapPoints(oldPts: array of JVector2; path: JPath): array of JVector2;
    property curves: array of JCurve;
    property bends: array of JPath;
    property autoClose: Jboolean;
  end;
  JPath = class external(JCurvePath)
  public
    constructor Create(points: JVector2);overload;
    procedure fromPoints(vectors: array of JVector2);
    procedure moveTo(x: integer; y: integer);
    procedure lineTo(x: integer; y: integer);
    procedure quadraticCurveTo(aCPx: integer; aCPy: integer; aX: integer; aY: integer);
    procedure bezierCurveTo(aCP1x: integer; aCP1y: integer; aCP2x: integer; aCP2y: integer; aX: integer; aY: integer);
    procedure splineThru(pts: array of JVector2);
    procedure arc(aX: integer; aY: integer; aRadius: integer; aStartAngle: integer; aEndAngle: integer; aClockwise: Jboolean);
    procedure absarc(aX: integer; aY: integer; aRadius: integer; aStartAngle: integer; aEndAngle: integer; aClockwise: Jboolean);
    procedure ellipse(aX: integer; aY: integer; xRadius: integer; yRadius: integer; aStartAngle: integer; aEndAngle: integer; aClockwise: Jboolean);
    procedure absellipse(aX: integer; aY: integer; xRadius: integer; yRadius: integer; aStartAngle: integer; aEndAngle: integer; aClockwise: Jboolean);
    function toShapes(): array of JShape;
    property actions: array of JPathActions;
  end;
  JGyroscope = class external(JObject3D)
  public
    constructor Create();
    procedure updateMatrixWorld(force: Jboolean);overload;
    property translationWorld: JVector3;
    property translationObject: JVector3;
    property rotationWorld: JQuaternion;
    property rotationObject: JQuaternion;
    property scaleWorld: JVector3;
    property scaleObject: JVector3;
  end;

  TShape_extractAllPoints_result_object = class;
  TShape_extractAllSpacedPoints_result_object = class;
  TShape_Utils_object = class;
  TShape_Utils_object_removeHoles_result_object = class;

  JShape = class external(JPath)
  public
    constructor Create(points: array of JVector2);overload;
    function extrude(options: variant): JExtrudeGeometry;overload;
    function makeGeometry(options: variant): JShapeGeometry;overload;
    function getPointsHoles(divisions: integer): array of array of JVector2;
    function getSpacedPointsHoles(divisions: integer): array of array of JVector2;
    function extractAllPoints(divisions: integer): TShape_extractAllPoints_result_object;
    function extractPoints(divisions: integer): array of JVector2;
    function extractAllSpacedPoints(divisions: JVector2): TShape_extractAllSpacedPoints_result_object;
    procedure fromPoints(vectors: array of JVector2);
    procedure moveTo(x: integer; y: integer);
    procedure lineTo(x: integer; y: integer);
    procedure quadraticCurveTo(aCPx: integer; aCPy: integer; aX: integer; aY: integer);
    procedure bezierCurveTo(aCP1x: integer; aCP1y: integer; aCP2x: integer; aCP2y: integer; aX: integer; aY: integer);
    procedure splineThru(pts: array of JVector2);
    procedure arc(aX: integer; aY: integer; aRadius: integer; aStartAngle: integer; aEndAngle: integer; aClockwise: Jboolean);
    procedure absarc(aX: integer; aY: integer; aRadius: integer; aStartAngle: integer; aEndAngle: integer; aClockwise: Jboolean);
    procedure ellipse(aX: integer; aY: integer; xRadius: integer; yRadius: integer; aStartAngle: integer; aEndAngle: integer; aClockwise: Jboolean);
    procedure absellipse(aX: integer; aY: integer; xRadius: integer; yRadius: integer; aStartAngle: integer; aEndAngle: integer; aClockwise: Jboolean);
    function toShapes(): array of JShape;
    procedure add(curve: JCurve);
    function checkConnection(): Jboolean;
    procedure closePath();
    function getBoundingBox(): JBoundingBox;
    function createPointsGeometry(divisions: integer): JGeometry;
    function createSpacedPointsGeometry(divisions: integer): JGeometry;
    function createGeometry(points: array of JVector2): JGeometry;
    procedure addWrapPath(bendpath: JPath);
    function getTransformedPoints(segments: integer; bends: JPath): array of JVector2;overload;
    function getTransformedSpacedPoints(segments: integer; bends: array of JPath): array of JVector2;overload;
    function getWrapPoints(oldPts: array of JVector2; path: JPath): array of JVector2;
    function getPoint(t: integer): JVector;
    function getPointAt(u: integer): JVector;
    function getPoints(divisions: integer): array of JVector;overload;
    function getSpacedPoints(divisions: integer): array of JVector;overload;
    function getLength(): integer;
    function getLengths(divisions: integer): array of integer;overload;
    procedure updateArcLengths();
    function getUtoTmapping(u: integer; distance: integer): integer;
    function getNormalVector(t: integer): JVector;
    function getTangent(t: integer): JVector;
    function getTangentAt(u: integer): JVector;
    property holes: array of JPath;
    property useSpacedPoints: Jboolean;
    property {static?} Utils: TShape_Utils_object;
    property actions: array of JPathActions;
    property curves: array of JCurve;
    property bends: array of JPath;
    property autoClose: Jboolean;
    property needsUpdate: Jboolean;
  end;
  TShape_extractAllPoints_result_object = class external "Object"
    property shape: array of JVector2;
    property holes: array of array of JVector2;
  end;
  TShape_extractAllSpacedPoints_result_object = class external "Object"
    property shape: array of JVector2;
    property holes: array of array of JVector2;
  end;
  TShape_Utils_object = class external "Object"
  public
    function removeHoles(contour: array of JVector2; holes: array of array of JVector2): TShape_Utils_object_removeHoles_result_object;
    function triangulateShape(contour: array of JVector2; holes: array of array of JVector2): array of JVector2;
    function isClockWise(pts: array of JVector2): Jboolean;
    function b2p0(t: integer; p: integer): integer;
    function b2p1(t: integer; p: integer): integer;
    function b2p2(t: integer; p: integer): integer;
    function b2(t: integer; p0: integer; p1: integer; p2: integer): integer;
    function b3p0(t: integer; p: integer): integer;
    function b3p1(t: integer; p: integer): integer;
    function b3p2(t: integer; p: integer): integer;
    function b3p3(t: integer; p: integer): integer;
    function b3(t: integer; p0: integer; p1: integer; p2: integer; p3: integer): integer;
  end;
  TShape_Utils_object_removeHoles_result_object = class external "Object"
    property shape: JShape;
    property isolatedPts: array of JVector2;
    property allpoints: array of JVector2;
  end;
  JAsteriskGeometry = class external(JGeometry)
  public
    constructor Create(innerRadius: integer; outerRadius: integer);
  end;
  JCircleGeometry = class external(JGeometry)
  public
    constructor Create(radius: integer);overload;
    constructor Create(radius: integer;segments: integer);overload;
    constructor Create(radius: integer;segments: integer;thetaStart: integer);overload;
    constructor Create(radius: integer;segments: integer;thetaStart: integer;thetaLength: integer);overload;
  end;
  JConvexGeometry = class external(JGeometry)
  public
    constructor Create(vertices: array of JVector3);
  end;
  JCubeGeometry = class external 'THREE.CubeGeometry' (JGeometry)
  public
    constructor Create(width: integer; height: integer; depth: integer);overload;
    constructor Create(width: integer; height: integer; depth: integer; widthSegments: integer);overload;
    constructor Create(width: integer; height: integer; depth: integer; widthSegments: integer;heightSegments: integer);overload;
    constructor Create(width: integer; height: integer; depth: integer; widthSegments: integer;heightSegments: integer;depthSegments: integer);overload;
  end;
  JCylinderGeometry = class external(JGeometry)
  public
    constructor Create(radiusTop: integer);overload;
    constructor Create(radiusTop: integer;radiusBottom: integer);overload;
    constructor Create(radiusTop: integer;radiusBottom: integer;height: integer);overload;
    constructor Create(radiusTop: integer;radiusBottom: integer;height: integer;radiusSegments: integer);overload;
    constructor Create(radiusTop: integer;radiusBottom: integer;height: integer;radiusSegments: integer;heightSegments: integer);overload;
    constructor Create(radiusTop: integer;radiusBottom: integer;height: integer;radiusSegments: integer;heightSegments: integer;openEnded: Jboolean);overload;
  end;

  TExtrudeGeometry_WorldUVGenerator_object = class;

  JExtrudeGeometry = class external(JGeometry)
  public
    constructor Create(shape: JShape);overload;
    constructor Create(shape: JShape;options: variant);overload;
    constructor Create(shapes: array of JShape);overload;
    constructor Create(shapes: array of JShape;options: variant);overload;
    procedure addShapeList(shapes: array of JShape; options: variant);overload;
    procedure addShape(shape: JShape; options: variant);overload;
    property shapebb: JBoundingBox;
    property {static?} WorldUVGenerator: TExtrudeGeometry_WorldUVGenerator_object;
  end;
  TExtrudeGeometry_WorldUVGenerator_object = class external "Object"
  public
    function generateTopUV(geometry: JGeometry; extrudedShape: JShape; extrudeOptions: JObject; indexA: integer; indexB: integer; indexC: integer): array of JVector2;
    function generateBottomUV(geometry: JGeometry; extrudedShape: JShape; extrudeOptions: JObject; indexA: integer; indexB: integer; indexC: integer): array of JVector2;
    function generateSideWallUV(geometry: JGeometry; extrudedShape: JShape; wallContour: JObject; extrudeOptions: JObject; indexA: integer; indexB: integer; indexC: integer; indexD: integer; stepIndex: integer; stepsLength: integer; contourIndex1: integer; contourIndex2: integer): array of JVector2;
  end;

  JPolyhedronGeometry = class external(JGeometry)
  public
    constructor Create(vertices: array of JVector3; faces: array of JFace; radius: integer);overload;
    constructor Create(vertices: array of JVector3; faces: array of JFace; radius: integer;detail: integer);overload;
  end;

  JIcosahedronGeometry = class external(JPolyhedronGeometry)
  public
    constructor Create(radius: integer; detail: integer);
  end;
  JLatheGeometry = class external(JGeometry)
  public
    constructor Create(points: array of JVector3; steps: integer);overload;
    constructor Create(points: array of JVector3; steps: integer;angle: integer);overload;
  end;
  JOctahedronGeometry = class external(JPolyhedronGeometry)
  public
    constructor Create(radius: integer; detail: integer);
  end;

  TParametricGeometry_constructor__func_ = function (u: integer; v: integer): JVector3;

  JParametricGeometry = class external(JGeometry)
  public
    constructor Create(func: TParametricGeometry_constructor__func_; slices: integer; stacks: integer; useTris: Jboolean);overload;
  end;
  JPlaneGeometry = class external(JGeometry)
  public
    constructor Create(width: integer; height: integer; widthSegments: integer);overload;
    constructor Create(width: integer; height: integer; widthSegments: integer;heightSegments: integer);overload;
    property width: integer;
    property height: integer;
    property widthSegments: integer;
    property heightSegments: integer;
  end;
  JShapeGeometry = class external(JGeometry)
  public
    constructor Create(shape: JShape; options: variant);overload;
    constructor Create(shapes: array of JShape; options: variant);overload;
    function addShapeList(shapes: array of JShape; options: variant): JShapeGeometry;
    procedure addShape(shape: JShape; options: variant);overload;
    property shapebb: JBoundingBox;
  end;
  JSphereGeometry = class external(JGeometry)
  public
    constructor Create(radius: integer; widthSegments: integer);overload;
    constructor Create(radius: integer; widthSegments: integer;heightSegments: integer);overload;
    constructor Create(radius: integer; widthSegments: integer;heightSegments: integer;phiStart: integer);overload;
    constructor Create(radius: integer; widthSegments: integer;heightSegments: integer;phiStart: integer;phiLength: integer);overload;
    constructor Create(radius: integer; widthSegments: integer;heightSegments: integer;phiStart: integer;phiLength: integer;thetaStart: integer);overload;
    constructor Create(radius: integer; widthSegments: integer;heightSegments: integer;phiStart: integer;phiLength: integer;thetaStart: integer;thetaLength: integer);overload;
    property radius: integer;
    property widthSegments: integer;
    property heightSegments: integer;
    property phiStart: integer;
    property phiLength: integer;
    property thetaStart: integer;
    property thetaLength: integer;
  end;
  JTetrahedronGeometry = class external(JPolyhedronGeometry)
  public
    constructor Create(radius: integer);overload;
    constructor Create(radius: integer;detail: integer);overload;
  end;
  JTextGeometryParameters = class external "Object"
    property size: integer;
    property height: integer;
    property curveSegments: integer;
    property font: string;
    property weight: string;
    property style: string;
    property bevelEnabled: Jboolean;
    property bevelThickness: integer;
    property bevelSize: integer;
  end;
  JTextGeometry = class external(JExtrudeGeometry)
  public
    constructor Create(text: string; TextGeometryParameters: JTextGeometryParameters);overload;
  end;
  JTorusGeometry = class external(JGeometry)
  public
    constructor Create(radius: integer);overload;
    constructor Create(radius: integer;tube: integer);overload;
    constructor Create(radius: integer;tube: integer;radialSegments: integer);overload;
    constructor Create(radius: integer;tube: integer;radialSegments: integer;tubularSegments: integer);overload;
    constructor Create(radius: integer;tube: integer;radialSegments: integer;tubularSegments: integer;arc: integer);overload;
    property radius: integer;
    property tube: integer;
    property radialSegments: integer;
    property tubularSegments: integer;
    property arc: integer;
  end;
  JTorusKnotGeometry = class external(JGeometry)
  public
    constructor Create(radius: integer);overload;
    constructor Create(radius: integer;tube: integer);overload;
    constructor Create(radius: integer;tube: integer;radialSegments: integer);overload;
    constructor Create(radius: integer;tube: integer;radialSegments: integer;tubularSegments: integer);overload;
    constructor Create(radius: integer;tube: integer;radialSegments: integer;tubularSegments: integer;p: integer);overload;
    constructor Create(radius: integer;tube: integer;radialSegments: integer;tubularSegments: integer;p: integer;q: integer);overload;
    constructor Create(radius: integer;tube: integer;radialSegments: integer;tubularSegments: integer;p: integer;q: integer;heightScale: integer);overload;
    property radius: integer;
    property tube: integer;
    property radialSegments: integer;
    property tubularSegments: integer;
    property p: integer;
    property q: integer;
    property heightScale: integer;
    property grid: array of array of integer;
  end;
  JTubeGeometry = class external(JGeometry)
  public
    constructor Create(path: JPath; segments: integer);overload;
    constructor Create(path: JPath; segments: integer;radius: integer);overload;
    constructor Create(path: JPath; segments: integer;radius: integer;radiusSegments: integer);overload;
    constructor Create(path: JPath; segments: integer;radius: integer;radiusSegments: integer;closed: Jboolean);overload;
    constructor Create(path: JPath; segments: integer;radius: integer;radiusSegments: integer;closed: Jboolean;debug: JObject3D);overload;
    constructor Create(path: JSplineCurve3; segments: integer);overload;
    constructor Create(path: JSplineCurve3; segments: integer;radius: integer);overload;
    constructor Create(path: JSplineCurve3; segments: integer;radius: integer;radiusSegments: integer);overload;
    constructor Create(path: JSplineCurve3; segments: integer;radius: integer;radiusSegments: integer;closed: Jboolean);overload;
    constructor Create(path: JSplineCurve3; segments: integer;radius: integer;radiusSegments: integer;closed: Jboolean;debug: Jboolean);overload;
    procedure FrenetFrames(path: JPath; segments: integer; closed: Jboolean);
    property path: JPath;
    property segments: integer;
    property radius: integer;
    property radiusSegments: integer;
    property closed: Jboolean;
    property grid: array of array of integer;
    property tangents: array of JVector3;
    property normals: array of JVector3;
    property binormals: array of JVector3;
  end;
  JAxisHelper = class external(JLine)
  public
    constructor Create(size: integer);
  end;
  JArrowHelper = class external(JObject3D)
  public
    constructor Create(dir: JVector3; origin: JVector3);overload;
    constructor Create(dir: JVector3; origin: JVector3;length_: integer);overload;
    constructor Create(dir: JVector3; origin: JVector3;length_: integer;hex: integer);overload;
    procedure setDirection(dir: JVector3);
    procedure setLength(length_: integer);
    procedure setColor(hex: integer);
    property line: JLine;
    property cone: JMesh;
  end;

  TCameraHelper_pointMap_object = class;

  JCameraHelper = class external(JLine)
  public
    constructor Create(camera: JCamera);
    property pointMap: TCameraHelper_pointMap_object;
    property camera: JCamera;
  end;
  TCameraHelper_pointMap_object = class external "Object"
  public
    function  GetItems(id: string): array of integer; external array;
    procedure SetItems(id: string; value: array of integer); external array;
//    property Items[id: string]: array of integer read GetItems write SetItems; default;
  end;
  JDirectionalLightHelper = class external(JObject3D)
  public
    constructor Create(light: JLight; sphereSize: integer; arrowLength: integer);
    property light: JLight;
    property direction: JVector3;
    property color: JColor;
    property lightArrow: JArrowHelper;
    property lightSphere: JMesh;
    property lightRays: JLine;
    property targetSphere: JMesh;
    property targetLine: JLine;
  end;
  JHemisphereLightHelper = class external(JObject3D)
  public
    constructor Create(light: JLight; sphereSize: integer; arrowLength: integer; domeSize: integer);
    property light: JLight;
    property color: JColor;
    property groundColor: JColor;
    property lightSphere: JMesh;
    property lightArrow: JArrowHelper;
    property lightArrowGround: JArrowHelper;
    property target: JVector3;
  end;
  JPointLightHelper = class external(JObject3D)
  public
    constructor Create(light: JLight; sphereSize: integer);
    property light: JLight;
    property color: JColor;
    property lightSphere: JMesh;
    property lightRays: JLine;
    property lightDistance: JMesh;
  end;
  JSpotLightHelper = class external(JObject3D)
  public
    constructor Create(light: JLight; sphereSize: integer; arrowLength: integer);
    property light: JLight;
    property direction: JVector3;
    property color: JColor;
    property lightArrow: JArrowHelper;
    property lightSphere: JMesh;
    property lightCone: JMesh;
    property lightRays: JLine;
    property gyroscope: JGyroscope;
    property targetSphere: JMesh;
    property targetLine: JLine;
  end;
  JImmediateRenderObject = class external(JObject3D)
  public
    constructor Create();
  end;
  JLensFlareProperty = class external "Object"
    property texture: JTexture;
    property size: integer;
    property distance: integer;
    property x: integer;
    property y: integer;
    property z: integer;
    property scale: integer;
    property rotation: integer;
    property opacity: integer;
    property color: JColor;
    property blending: JBlending;
  end;

  TLensFlare_customUpdateCallback_ = procedure (object_: JLensFlare);

  JLensFlare = class external(JObject3D)
  public
    constructor Create(texture: JTexture);overload;
    constructor Create(texture: JTexture;size: integer);overload;
    constructor Create(texture: JTexture;size: integer;distance: integer);overload;
    constructor Create(texture: JTexture;size: integer;distance: integer;blending: JBlending);overload;
    constructor Create(texture: JTexture;size: integer;distance: integer;blending: JBlending;color: JColor);overload;
    procedure add(object_: JObject3D);overload;
    procedure add(texture: JTexture);overload;
    procedure add(texture: JTexture;size: integer);overload;
    procedure add(texture: JTexture;size: integer;distance: integer);overload;
    procedure add(texture: JTexture;size: integer;distance: integer;blending: JBlending);overload;
    procedure add(texture: JTexture;size: integer;distance: integer;blending: JBlending;color: JColor);overload;
    procedure add(texture: JTexture;size: integer;distance: integer;blending: JBlending;color: JColor;opacity: integer);overload;
    procedure updateLensFlares();
    property lensFlares: array of JLensFlareProperty;
    property positionScreen: JVector3;
    property customUpdateCallback: TLensFlare_customUpdateCallback_;
  end;
  JMorphBlendMeshAnimation = class external "Object"
    property startFrame: integer;
    property endFrame: integer;
    property length: integer;
    property fps: integer;
    property duration: integer;
    property lastFrame: integer;
    property currentFrame: integer;
    property active: Jboolean;
    property time: integer;
    property direction: integer;
    property weight: integer;
    property directionBackwards: Jboolean;
    property mirroredLoop: Jboolean;
  end;

  TMorphBlendMesh_animationsMap_object = class;

  JMorphBlendMesh = class external(JMesh)
  public
    constructor Create(geometry: JGeometry; material: JMaterial);
    procedure createAnimation(name: string; start: integer; &end: integer; fps: integer);
    procedure autoCreateAnimations(fps: integer);
    procedure setAnimationDirectionForward(name: string);
    procedure setAnimationDirectionBackward(name: string);
    procedure setAnimationFPS(name: string; fps: integer);
    procedure setAnimationDuration(name: string; duration: integer);
    procedure setAnimationWeight(name: string; weight: integer);
    procedure setAnimationTime(name: string; time: integer);
    function getAnimationTime(name: string): integer;
    function getAnimationDuration(name: string): integer;
    procedure playAnimation(name: string);
    procedure stopAnimation(name: string);
    procedure update(delta: integer);
    property animationsMap: TMorphBlendMesh_animationsMap_object;
    property animationsList: array of JMorphBlendMeshAnimation;
    property firstAnimation: string;
  end;
  TMorphBlendMesh_animationsMap_object = class external "Object"
  public
    function  GetItems(name: string): JMorphBlendMeshAnimation; external array;
    procedure SetItems(name: string; value: JMorphBlendMeshAnimation); external array;
    property Items[name: string]: JMorphBlendMeshAnimation read GetItems write SetItems; default;
  end;
  JDepthPassPlugin = class external(JRendererPlugin)
  public
    constructor Create();
    procedure init(renderer: JRenderer);
    procedure render(scene: JScene; camera: JCamera);
    procedure update(scene: JScene; camera: JCamera);
    property enabled: Jboolean;
    property renderTarget: JRenderTarget;
  end;
  JLensFlarePlugin = class external(JRendererPlugin)
  public
    constructor Create();
    procedure init(renderer: JRenderer);
    procedure render(scene: JScene; camera: JCamera; viewportWidth: integer; viewportHeight: integer);
  end;
  JShadowMapPlugin = class external(JRendererPlugin)
  public
    constructor Create();
    procedure init(renderer: JRenderer);
    procedure render(scene: JScene; camera: JCamera);
    procedure update(scene: JScene; camera: JCamera);
  end;
  JSpritePlugin = class external(JRendererPlugin)
  public
    constructor Create();
    procedure init(renderer: JRenderer);
    procedure render(scene: JScene; camera: JCamera; viewportWidth: integer; viewportHeight: integer);
  end;

  TShaderLibrary_Items_result_object = class;

  JShaderLibrary = class external "Object"
  public
    function  GetItems(name: string): TShaderLibrary_Items_result_object; external array;
    procedure SetItems(name: string; value: TShaderLibrary_Items_result_object); external array;
    property Items[name: string]: TShaderLibrary_Items_result_object read GetItems write SetItems; default;
  end;
  TShaderLibrary_Items_result_object = class external "Object"
    property vertexShader: string;
    property fragmentShader: string;
  end;
  JUniformsUtils = class external "UniformsUtils"
  public
    function merge(uniforms: array of JObject): JUniforms;overload;
    function merge(uniforms: array of JUniforms): JUniforms;overload;
    function clone(uniforms_src: JUniforms): JUniforms;
  end;
  JUniformsLib = class external "UniformsLib"
    property common: JUniforms;
    property bump: JUniforms;
    property normalmap: JUniforms;
    property fog: JUniforms;
    property lights: JUniforms;
    property particle: JUniforms;
    property shadowmap: JUniforms;
  end;
  JShader = class external "Object"
    property uniforms: JUniforms;
    property vertexShader: string;
    property fragmentShader: string;
  end;
  JShaderLib = class external "ShaderLib"
  public
    function  GetItems(name: string): JShader; external array;
    procedure SetItems(name: string; value: JShader); external array;
    property Items[name: string]: JShader read GetItems write SetItems; default;
    property basic: JShader;
    property lambert: JShader;
    property phong: JShader;
    property particle_basic: JShader;
    property depth: JShader;
    property dashed: JShader;
    property normal: JShader;
    property normalmap: JShader;
    property cube: JShader;
    property depthRGBA: JShader;
  end;
  JTHREE_Exports = class external
    property REVISION: string;
    property CullFaceNone: JCullFace;
    property CullFaceBack: JCullFace;
    property CullFaceFront: JCullFace;
    property CullFaceFrontBack: JCullFace;
    property FrontFaceDirectionCW: JFrontFaceDirection;
    property FrontFaceDirectionCCW: JFrontFaceDirection;
    property BasicShadowMap: JShadowMapType;
    property PCFShadowMap: JShadowMapType;
    property PCFSoftShadowMap: JShadowMapType;
    property FrontSide: JSide;
    property BackSide: JSide;
    property DoubleSide: JSide;
    property NoShading: JShading;
    property FlatShading: JShading;
    property SmoothShading: JShading;
    property NoColors: JColors;
    property FaceColors: JColors;
    property VertexColors: JColors;
    property NoBlending: JBlending;
    property NormalBlending: JBlending;
    property AdditiveBlending: JBlending;
    property SubtractiveBlending: JBlending;
    property MultiplyBlending: JBlending;
    property CustomBlending: JBlending;
    property AddEquation: JBlendingEquation;
    property SubtractEquation: JBlendingEquation;
    property ReverseSubtractEquation: JBlendingEquation;
    property ZeroFactor: JBlendingDstFactor;
    property OneFactor: JBlendingDstFactor;
    property SrcColorFactor: JBlendingDstFactor;
    property OneMinusSrcColorFactor: JBlendingDstFactor;
    property SrcAlphaFactor: JBlendingDstFactor;
    property OneMinusSrcAlphaFactor: JBlendingDstFactor;
    property DstAlphaFactor: JBlendingDstFactor;
    property OneMinusDstAlphaFactor: JBlendingDstFactor;
    property DstColorFactor: JBlendingSrcFactor;
    property OneMinusDstColorFactor: JBlendingSrcFactor;
    property SrcAlphaSaturateFactor: JBlendingSrcFactor;
    property MultiplyOperation: JCombine;
    property MixOperation: JCombine;
    property AddOperation: JCombine;
    property UVMapping: JMappingConstructor;
    property CubeReflectionMapping: JMappingConstructor;
    property CubeRefractionMapping: JMappingConstructor;
    property SphericalReflectionMapping: JMappingConstructor;
    property SphericalRefractionMapping: JMappingConstructor;
    property RepeatWrapping: JWrapping;
    property ClampToEdgeWrapping: JWrapping;
    property MirroredRepeatWrapping: JWrapping;
    property NearestFilter: JTextureFilter;
    property NearestMipMapNearestFilter: JTextureFilter;
    property NearestMipMapLinearFilter: JTextureFilter;
    property LinearFilter: JTextureFilter;
    property LinearMipMapNearestFilter: JTextureFilter;
    property LinearMipMapLinearFilter: JTextureFilter;
    property UnsignedByteType: JTextureDataType;
    property ByteType: JTextureDataType;
    property ShortType: JTextureDataType;
    property UnsignedShortType: JTextureDataType;
    property IntType: JTextureDataType;
    property UnsignedIntType: JTextureDataType;
    property FloatType: JTextureDataType;
    property UnsignedShort4444Type: JPixelType;
    property UnsignedShort5551Type: JPixelType;
    property UnsignedShort565Type: JPixelType;
    property AlphaFormat: JPixelFormat;
    property RGBFormat: JPixelFormat;
    property RGBAFormat: JPixelFormat;
    property LuminanceFormat: JPixelFormat;
    property LuminanceAlphaFormat: JPixelFormat;
    property RGB_S3TC_DXT1_Format: JCompressedPixelFormat;
    property RGBA_S3TC_DXT1_Format: JCompressedPixelFormat;
    property RGBA_S3TC_DXT3_Format: JCompressedPixelFormat;
    property RGBA_S3TC_DXT5_Format: JCompressedPixelFormat;
    ColorKeywords: TColorKeywords;
    GeometryIdCount: integer;
    GeometryLibrary: array of JGeometry;
    property Math: JMath;
    Object3DIdCount: integer;
    Object3DLibrary: array of JObject3D;
    MaterialLibrary: array of JMaterial;
    MaterialIdCount: integer;
    LineStrip: JLineType;
    LinePieces: JLineType;
    property ShaderChunk: JShaderChunk;
    TextureIdCount: integer;
    TextureLibrary: array of JTexture;
    property FontUtils: JFontUtils;
    property GeometryUtils: JGeometryUtils;
    property ImageUtils: JImageUtils;
    property SceneUtils: JSceneUtils;
    property AnimationHandler: JAnimationHandler;
    property ShaderFlares: JShaderLibrary;
    property ShaderSprite: JShaderLibrary;
    property UniformsUtils: JUniformsUtils;
    property UniformsLib: JUniformsLib;
    property ShaderLib: JShaderLib;
  end;

function FontUtils_JFontUtils_Triangulate_object(contour: array of JVector2;indices: Jboolean): array of JVector2;external;


implementation

end.

