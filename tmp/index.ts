const url = "http://127.0.0.1:8081/users";

const json = { fullName: "0", age: 30 };

await fetch(url).then(async r => console.log(await r.text()));

await fetch(url, {
  method: "POST",
  body: JSON.stringify(json),
  headers: {
    Cookie: "session-id=poipoi-x",
    "Content-Type": "application/json",
  },
}).then(async r => {
  console.log(await r.text());
});
