//import 'bootstrap/dist/css/bootstrap.min.css'
import 'bootswatch/dist/darkly/bootstrap.min.css'
import './styles.css'
import uuidv4 from 'uuid/v4'
import '@fortawesome/fontawesome-free/js/all'
import { Elm } from './Main.elm'
import ApolloClient from 'apollo-client'
import { split } from 'apollo-link'
import { HttpLink } from 'apollo-link-http'
import { WebSocketLink } from 'apollo-link-ws'
import { getMainDefinition } from 'apollo-utilities'
import { InMemoryCache } from 'apollo-cache-inmemory'
import gql from 'graphql-tag'

const GRAPHQL_URI = process.env.ELM_APP_GRAPHQL_URI //'salamander-hasura.herokuapp.com/v1/graphql'
const CMD_URI = process.env.ELM_APP_CMD_URI // 'cosmic-ray-api.herokuapp.com'

const getClient = () => {
  // Create an http link:
  const httpLink = new HttpLink({
    uri: `https://${GRAPHQL_URI}`
  })

  // Create a WebSocket link:
  const wsLink = new WebSocketLink({
    uri: `ws://${GRAPHQL_URI}`,
    options: {
      reconnect: true,
      lazy: true
    }
  })

  // using the ability to split links, you can send data to each link
  // depending on what kind of operation is being sent
  const link = split(
    // split based on operation type
    ({ query }) => {
      const definition = getMainDefinition(query)
      return (
        definition.kind === 'OperationDefinition' &&
        definition.operation === 'subscription'
      )
    },
    wsLink,
    httpLink
  )
  const client = new ApolloClient({
    link: link,
    cache: new InMemoryCache({
      addTypename: true
    })
  })
  return client
}

var clientId = window.localStorage.getItem('clientId')

if (typeof clientId === 'undefined' || clientId == null) {
  clientId = uuidv4()
  window.localStorage.setItem('clientId', clientId)
}

const subscribe = (data, callback) => {
  getClient().subscribe({
    query: gql`${data}`,
    variables: {}
  }).subscribe({
    next(resp) {
      callback(resp)
    },
    error(err) {
      console.log('error is')
      console.log(err)
    }
  })
}

document.addEventListener('DOMContentLoaded', function () {
  var app = Elm.Main.init({
    node: document.getElementById('root'),
    flags: { clientId: clientId, queryEndpoint: `http://${GRAPHQL_URI}`, commandEndpoint: `http://${CMD_URI}` }
  })

  app.ports.createGamesSubscription.subscribe(function (data) {
    subscribe(data, function (resp) { app.ports.gamesReceived.send(resp) })
  })

  app.ports.createGameSubscription.subscribe(function (data) {
    subscribe(data, function (resp) { app.ports.gameReceived.send(resp) })
  })

  app.ports.createChallengesSubscription.subscribe(function (data) {
    subscribe(data, function (resp) { app.ports.challengesReceived.send(resp) })
  })

  app.ports.createMyChallengesSubscription.subscribe(function (data) {
    subscribe(data, function (resp) { app.ports.myChallengesReceived.send(resp) })
  })

  app.ports.createMyGamesSubscription.subscribe(function (data) {
    subscribe(data, function (resp) { app.ports.myGamesReceived.send(resp) })
  })

  app.ports.createChallengesSubscriptionGame.subscribe(function (data) {
    subscribe(data, function (resp) { app.ports.challengesReceivedGame.send(resp) })
  })
})
