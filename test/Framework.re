let get = () => {
  switch (Sys.getenv_opt("REASON_NATIVE_ROOT")) {
  | Some(dir) => dir
  | None => "."
  };
};

let projectDir = get();

include Rely.Make({
  let config =
    Rely.TestFrameworkConfig.initialize({
      snapshotDir: "__snapshots__",
      projectDir,
    });
});
