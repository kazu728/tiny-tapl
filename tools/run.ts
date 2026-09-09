import { parse } from "npm:tiny-ts-parser";

const source = Deno.args.join(" ") ||
  await new Response(Deno.stdin.readable).text();

type Port = { subscribe: (fn: (value: string) => void) => void };
const scope = {} as {
  Elm: {
    Main: {
      init: (config: { flags: ReturnType<typeof parse> }) => {
        ports: { stdout: Port; stderr: Port };
      };
    };
  };
};

new Function(await Deno.readTextFile(new URL("../elm/build/main.js", import.meta.url))).call(scope);

const app = scope.Elm.Main.init({ flags: parse(source) });

try {
  console.log(
    await new Promise<string>((resolve, reject) => {
      app.ports.stdout.subscribe(resolve);
      app.ports.stderr.subscribe((message: string) => reject(new Error(message)));
    }),
  );
} catch (e) {
  console.error((e as Error).message);
  Deno.exit(1);
}
