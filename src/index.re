open Reprocessing;

let createVelocity = (max: float): (float, float) => (
  Random.float(max),
  Random.float(max),
);

let createPosition =
    ((width, height) as _display: (float, float)): (float, float) => (
  Random.float(width),
  Random.float(height),
);

let display = (640., 480.);

module Edible = {
  let defaultWidth = 10.;
  let defaultHeight = 10.;
  let maximumVelocity = 0.1;

  type t = {
    position: (float, float),
    velocity: (float, float),
    width: float,
    height: float,
  };

  let make = (area: (float, float)): t => {
    position: createPosition(area),
    velocity: createVelocity(maximumVelocity),
    width: defaultWidth,
    height: defaultHeight,
  };

  let rec create = (area: (float, float), length: int): list(t) =>
    if (length <= 0) {
      [];
    } else {
      [make(area)] @ create(area, length - 1);
    };

  let update =
      (edibles: list(t), (width, height) as display: (float, float), _env)
      : list(t) => {
    List.map(
      ({position: (x, y), velocity: (xSpeed, ySpeed)} as edible) => {
        let (x', y') = (x +. xSpeed, y +. ySpeed);
        if (x' < 0. || width < x' || y' < 0. || height < y') {
          List.hd(create(display, 1));
        } else {
          {...edible, position: (x', y'), velocity: (xSpeed, ySpeed)};
        };
      },
      edibles,
    );
  };

  let draw = (edibles: list(t), env) => {
    List.iter(
      ({position, width, height, velocity: (_xSpeed, _ySpeed)}) => {
        Draw.stroke(Constants.green, env);
        Draw.fill(Constants.white, env);
        Draw.rectf(~pos=position, ~width, ~height, env);
      },
      edibles,
    );
  };
};

module Sandworm = {
  let sandwormSpeed = 1.;
  let maximumVelocity = 0.5;
  let segmentSep = 7.;
  let segmentRadius = 5.;

  type stateT =
    | Moving
    | Eating
    | Stopped;

  type headT = {
    state: stateT,
    target: (float, float),
    position: (float, float),
    velocity: (float, float),
  };

  type bodyT = {position: (float, float)};

  type t =
    | Head(headT)
    | Body(t, bodyT);

  let rec makeBody = (parent: t, len: int): t =>
    if (len > 0) {
      let (x, y) =
        switch (parent) {
        | Head({position, _})
        | Body(_, {position}) => position
        };
      makeBody(Body(parent, {position: (x -. segmentSep, y)}), len - 1);
    } else {
      parent;
    };

  let make = (display: (float, float), length: int): t => {
    let position = createPosition(display);
    makeBody(
      Head({
        state: Stopped,
        target: position,
        position,
        velocity: createVelocity(maximumVelocity),
      }),
      length,
    );
  };

  let rec draw = (sandworm: t, env) => {
    let ((x, y), color) =
      switch (sandworm) {
      | Head({state, position, _}) =>
        let color =
          switch (state) {
          | Moving => Constants.green
          | Stopped => Constants.blue
          | Eating => Constants.red
          };

        (position, color);
      | Body(parent, {position, _}) =>
        draw(parent, env);
        (position, Constants.blue);
      };
    Draw.stroke(color, env);
    Draw.fill(Constants.white, env);
    Draw.ellipsef(
      ~center=(x, y),
      ~radx=segmentRadius,
      ~rady=segmentRadius,
      env,
    );
  };

  let updateHead =
      (
        (xTarget, yTarget) as _target: (float, float),
        (x, y) as _position: (float, float),
        (xSpeed, ySpeed) as velocity: (float, float),
        edibles: list(Edible.t),
        display: (float, float),
        env,
      )
      : (t, list(Edible.t)) => {
    let (xMouse, yMouse) = Env.pmouse(env);
    let (xTarget, yTarget) as target =
      Env.mousePressed(env)
        ? (float_of_int(xMouse), float_of_int(yMouse)) : (xTarget, yTarget);

    let a' = xTarget -. x;
    let b' = yTarget -. y;
    let c' = sqrt(a' ** 2. +. b' ** 2.);

    let c = sqrt(xSpeed ** 2. +. ySpeed ** 2.);

    let r = c /. c';
    let a = a' *. r;
    let b = b' *. r;

    let changedVelocity = (a, b);
    let stopped =
      abs_float(xTarget -. x) < 1. && abs_float(yTarget -. y) < 1.;
    let position' = stopped ? (xTarget, yTarget) : (x +. a, y +. b);

    if (stopped) {
      let (eaten, edibles) =
        List.partition(
          (edible: Edible.t) =>
            Utils.intersectRectCircle(
              ~rectPos=edible.position,
              ~rectW=edible.width,
              ~rectH=edible.height,
              ~circlePos=position',
              ~circleRad=segmentRadius,
            ),
          edibles,
        );
      let no_eaten = List.length(eaten);
      let eating = no_eaten > 0;
      // if close to target don't stop
      let state = eating ? Eating : Stopped;
      //Js.log(no_eaten);
      (
        Head({state, target: position', position: position', velocity}),
        edibles @ Edible.create(display, no_eaten),
      );
    } else {
      (
        Head({
          state: Moving,
          target,
          position: position',
          velocity: changedVelocity,
        }),
        edibles,
      );
    };
  };

  let updateBody = (parent: t, position): t => {
    let (x, y) = position;

    let (xTarget, yTarget) =
      switch (parent) {
      | Head({position: t, _})
      | Body(_, {position: t}) => t
      };

    // find direction from position to target
    let a' = xTarget -. x;
    let b' = yTarget -. y;

    // find distance to target
    let c' = sqrt(a' ** 2. +. b' ** 2.);

    let c = segmentSep;

    let r = c /. c';
    let a = a' *. r;
    let b = b' *. r;

    // find distance from parent in straight line to current position
    let position' = (xTarget -. a, yTarget -. b);

    Body(parent, {position: position'});
  };

  let rec update =
          (
            sandworm: t,
            edibles: list(Edible.t),
            display: (float, float),
            env,
          )
          : (t, list(Edible.t)) => {
    switch (sandworm) {
    | Head({target, position, velocity}) =>
      updateHead(target, position, velocity, edibles, display, env)
    | Body(parent, {position: (x, y)}) =>
      let (parent', edibles) = update(parent, edibles, display, env);
      (updateBody(parent', (x, y)), edibles);
    };
  };
};

type globalStateT = {
  display: (float, float),
  edibles: list(Edible.t),
  sandworm: Sandworm.t,
  image: imageT,
  font: fontT,
  score: int,
};

let createEdible = Edible.create(display);

let setup = env => {
  let (width, height) = display;
  Env.size(~width=int_of_float(width), ~height=int_of_float(height), env);
  Draw.background(Constants.white, env);
  {
    display,
    edibles: createEdible(10),
    sandworm: Sandworm.make(display, 15),
    image: Draw.loadImage(~filename="assets/flappy.png", ~isPixel=true, env),
    font: Draw.loadFont(~filename="assets/flappy.fnt", ~isPixel=true, env),
    score: 0,
  };
};

let draw = ({edibles, sandworm, _} as state, env) => {
  Draw.background(Constants.white, env);
  Draw.clear(env);
  Edible.draw(edibles, env);
  Sandworm.draw(sandworm, env);

  let (sandworm', edibles') =
    Sandworm.update(sandworm, edibles, display, env);
  {
    ...state,
    sandworm: sandworm',
    edibles: Edible.update(edibles', display, env),
  };
};

run(~setup, ~draw, ());
