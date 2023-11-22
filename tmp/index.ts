import createClient from "openapi-fetch";
import { paths } from "./p";

const url = "http://127.0.0.1:8080";

const openapiClient = createClient<paths>({ baseUrl: url });

const headers = new Headers();
headers.set("cookie", "session-id=xxx");

const r = await openapiClient.GET("/users/{user-id}", {
  params: {
    path: { "user-id": "01HE8J6FXQNK3V36RNZGVH95MX!" },
  },
  headers,
});

if (r.data) {
  console.log(r.data);
} else {
  console.log(r.response.status);
}
