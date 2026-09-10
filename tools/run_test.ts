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
    ["(x: number) => x", "function"],
    ["((x: number) => x)(1)", "number"],
    ["const x = 1; x + 1", "number"],
    ["1 + 1; true", "boolean"],
    ["const f = (x: number) => x; f(1)", "number"],
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
  const cases = [
    ["true ? 1 : false", "then and else have different types"],
    ["((x: number) => x)(true)", "parameter type mismatch"],
    ["x + 1", "unknown variable: x"],
    ["(x: boolean) => x + 1", "number expected"],
    ["const x = true; x + 1", "number expected"],
    ["x; 1", "unknown variable: x"],
  ] as const;

  for (const [source, expected] of cases) {
    const output = await run(source);
    assertEquals(output.code, 1);
    assertEquals(output.stdout, "");
    assertStringIncludes(output.stderr, expected);
  }
});