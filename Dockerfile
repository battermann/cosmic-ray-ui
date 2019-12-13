# Build Stage
FROM node:13.4.0-buster as build
USER node
ENV NPM_CONFIG_PREFIX=/home/node/.npm-global
ENV PATH=$PATH:/home/node/.npm-global/bin
ARG ELM_APP_GRAPHQL_URI
ARG ELM_APP_CMD_URI
ENV ELM_APP_GRAPHQL_URI=$ELM_APP_GRAPHQL_URI
ENV ELM_APP_CMD_URI=$ELM_APP_CMD_URI
WORKDIR /app
COPY ./elm.json ./elm.json
COPY ./src ./src
COPY ./public ./public
COPY ./package.json ./package.json
COPY ./package-lock.json ./package-lock.json
USER root
RUN npm install --unsafe-perm=true
RUN npx elm-app build

# Runtime Stage
FROM nginx:1.17.6-alpine
COPY --from=build /app/build/ /usr/share/nginx/html
COPY nginx.conf /etc/nginx/conf.d/default.conf
