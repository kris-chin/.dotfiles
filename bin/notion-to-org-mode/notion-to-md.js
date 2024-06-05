/*
This script assumes that the following are globally installed:
  1. `@notionhq/client` - allows for notion API integration
  2. `notion-to-md` - converts notion to md
  3. `minimist` - parsing arguments with flags

Additionally, a Notion integration and api token must be created
*/
const { Client } = require("@notionhq/client")
const { NotionToMarkdown } = require("notion-to-md")
const fs = require("fs")
const minimist = require("minimist")

/**
  Args:
  --page_name="page name query" - the page name. this will get converted to a page_id
  --page_id="page id" - a page id. skips the notion API search (which.. is strange?)
  --token='integration token' - the notion token
*/
const argv = minimist(process.argv.slice(2))

if (argv.token == null || (argv.page_name == null && argv.page_id == null)) {
  console.error("Required params: (--page_name or --page_id), --token")
  return
}

//TODO: Provide the integration token as an argument
const notionClient = new Client({
  auth: argv.token
});

//Authentiate notion
const notion2Markdown = new NotionToMarkdown({ notionClient })

const getMarkdown = async () => {

  //Try to get page_id via search
  let page_id = argv.page_id

  if (page_id == null) {
    const res = await notionClient.search({
      query: argv.page_name,
      page_size: 5
    })

    const results = res.results.map((result) => {
      return result.url
    })

    console.log("results: ", results);
  }

  //Convert page-id to markdown
  const mdblocks = await notion2Markdown.pageToMarkdown(page_id);
  const mdString = notion2Markdown.toMarkdownString(mdblocks);
  console.log(mdString.parent);

}

getMarkdown()
