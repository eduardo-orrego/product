package com.nttdata.product;

import com.nttdata.product.api.ProductController;
import com.nttdata.product.api.request.ProductRequest;
import com.nttdata.product.business.ProductService;
import com.nttdata.product.enums.ProductTypeEnum;
import com.nttdata.product.model.Product;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mockito;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.reactive.AutoConfigureWebTestClient;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.http.MediaType;
import org.springframework.test.web.reactive.server.WebTestClient;
import reactor.core.publisher.Mono;

@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.MOCK)
@AutoConfigureWebTestClient(timeout = "20000")
public class ProductControllerTest {
  private static final String URI_GET = "/api/products";
  private static final String URI_POST = "/api/products";
  private static final String URI_PUT = "/api/products/{productId}}";
  private static final String URI_DELETE = "/api/products/{productId}}";

  @Autowired
  private WebTestClient webTestClient;
  private static Product product;
  private static ProductRequest productRequest;

  @InjectMocks
  private ProductController productController;

  @MockBean
  private ProductService productService;

  @BeforeAll
  static void setup2() {
    product = ModelMock.getProduct();
    productRequest = ModelMock.getProductRequest();
  }

  @Test
  @DisplayName("Retorna exito")
  void productsGet() {

    String productTypeParam = ProductTypeEnum.SAVINGS.name();

    Mockito.when(productService.getProduct(Mockito.anyString()))
      .thenReturn(Mono.just(product));

    webTestClient.get()
      .uri(uriBuilder -> uriBuilder
        .path(URI_GET)
        .queryParam("productType", productTypeParam)
        .build())
      .accept(MediaType.APPLICATION_JSON)
      .exchange()
      .expectStatus().isOk()
      .expectBody(Product.class);

  }

  @Test
  void productsPost() {

    Mockito.when(productService.saveProduct(Mockito.any()))
      .thenReturn(Mono.just(product));

    webTestClient.post()
      .uri(URI_POST)
      .accept(MediaType.APPLICATION_JSON)
      .bodyValue(productRequest)
      .exchange()
      .expectStatus().isCreated()
      .expectBody(Product.class);

  }

  @Test
  void productsPut() {

    String productIdParam = "id";

    Mockito.when(productService.updateProduct(Mockito.any(), Mockito.anyString()))
      .thenReturn(Mono.just(product));

    webTestClient.put()
      .uri(URI_PUT, productIdParam)
      .accept(MediaType.APPLICATION_JSON)
      .bodyValue(productRequest)
      .exchange()
      .expectStatus().isOk()
      .expectBody(Product.class);
  }

  @Test
  void productDelete() {

    String productIdParam = "id";

    Mockito.when(productService.deleteProduct(Mockito.anyString()))
      .thenReturn(Mono.empty());

    webTestClient.delete()
      .uri(URI_DELETE, productIdParam)
      .accept(MediaType.APPLICATION_JSON)
      .exchange()
      .expectStatus().isOk()
      .expectBody()
      .isEmpty();
  }
}
