import createClient from "openapi-fetch";
import { paths } from "./p";

const url = "http://127.0.0.1:8081/users";

const openapiClient = createClient<paths>({ baseUrl: "https://myapi.dev/v1/" });

const {
  data, // only present if 2XX response
  error, // only present if 4XX or 5XX response
} = await openapiClient.GET("/pet/findByStatus", {
  params: {
    query: { status: "sold" },
  },
});

await openapiClient.PUT("/pet", {
  body: {
    name: "",
    photoUrls: [],
  },
});

const json = { fullName: "0", age: 30 };

const f = async () => {
  await fetch(url).then(async r => console.log(await r.text()));
};

await Promise.all([
  f(),
  //  f(), f(), f(), f(), f()
]);

// await fetch(url, {
//   method: "POST",
//   body: JSON.stringify(json),
//   headers: {
//     Cookie: "session-id=poipoi-x",
//     "Content-Type": "application/json",
//   },
// }).then(async r => {
//   console.log(await r.text());
// });
