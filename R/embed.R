embed <- function(texts, hostname = "192.168.2.118", port = "11434") {
  request(paste0("http://", hostname, ":", port, "/api/embed")) |>
    req_body_json(list(model = "qwen3-embedding:0.6b", input = texts)) |>
    req_perform() |> resp_body_json() |> (\(r) r$embeddings)()
}
