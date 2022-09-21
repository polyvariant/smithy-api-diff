$version: "2"
namespace demo

service MyService {
  operations: [MyOp]
}

operation MyOp {
  input := {
    @required
    @httpPayload
    payload: Payload
  }
}

structure Payload {
  @required size: Integer,
  @required data: Data
}

union Data {
  a: A,
  b: B
}

structure A {
  @required a: String
}

structure B {
  b: Integer
}
