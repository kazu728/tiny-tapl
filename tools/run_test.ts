import { assertEquals, assertStringIncludes } from "@std/assert";

const decoder = new TextDecoder();
const home = Deno.env.get("HOME");

async function run(source: string) {
  const output = await new Deno.Command("deno", {
    args: ["task", "--quiet", "run:compiled", source],
    clearEnv: true,
    env: home ? { HOME: home } : {},
    stdout: "piped",
    stderr: "piped",
  }).output();

  return {
    code: output.code,
    stdout: decoder.decode(output.stdout).trim(),
    stderr: decoder.decode(output.stderr).trim(),
  };
}

Deno.test("tiny-ts-parser の AST を Elm で型検査する", async (test) => {
  const cases = [
    ["true", "boolean"],
    ["false", "boolean"],
    ["1 + 2", "number"],
    ["true ? 1 : 2", "number"],
  ] as const;

  for (const [source, expected] of cases) {
    await test.step(source, async () => {
      assertEquals(await run(source), {
        code: 0,
        stdout: expected,
        stderr: "",
      });
    });
  }
});

Deno.test("型エラーを拒否する", async () => {
  const output = await run("true ? 1 : false");

  assertEquals(output.code, 1);
  assertEquals(output.stdout, "");
  assertStringIncludes(output.stderr, "then and else have different types");
});

Deno.test("basic にない構文を Elm の境界で拒否する", async () => {
  const output = await run("(x: number) => x");

  assertEquals(output.code, 1);
  assertEquals(output.stdout, "");
  assertStringIncludes(
    output.stderr,
    "basic では扱えない項です: func",
  );
});
