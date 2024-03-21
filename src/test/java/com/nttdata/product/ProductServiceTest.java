package com.nttdata.product;

import com.nttdata.product.api.request.ProductRequest;
import com.nttdata.product.business.impl.ProductServiceImpl;
import com.nttdata.product.enums.ProductTypeEnum;
import com.nttdata.product.model.Product;
import com.nttdata.product.repository.ProductRepository;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

@ExtendWith(MockitoExtension.class)
public class ProductServiceTest {

  private static Product product;
  private static Product productDefault;
  private static ProductRequest productRequest;
  private static ProductRequest productRequestDefault;

  @InjectMocks
  private ProductServiceImpl productService;

  @Mock
  private ProductRepository productRepository;

  @BeforeAll
  static void initialSetup() {
    product = ModelMock.getProduct();
    productDefault = ModelMock.getProductDefault();
    productRequest = ModelMock.getProductRequest();
    productRequestDefault = ModelMock.getProductRequestDefault();
  }

  @Test
  void getProduct() {

    String productTypeParam = ProductTypeEnum.SAVINGS.name();

    Mockito.when(productRepository.findProduct(Mockito.anyString()))
      .thenReturn(Mono.just(product));

    StepVerifier.create(productService.getProduct(productTypeParam))
      .expectNext(product)
      .expectComplete()
      .verify();

    Mockito.verify(productRepository, Mockito.times(1))
      .findProduct(productTypeParam);

  }

  @Test
  void saveProduct() {

    Mockito.when(productRepository.findExistsProduct(Mockito.anyString()))
      .thenReturn(Mono.just(false));

    Mockito.when(productRepository.saveProduct(Mockito.any()))
      .thenReturn(Mono.just(productDefault));

    StepVerifier.create(productService.saveProduct(productRequestDefault))
      .expectNext(productDefault)
      .expectComplete()
      .verify();

  }

  @Test
  void saveProductError() {

    Mockito.when(productRepository.findExistsProduct(Mockito.anyString()))
      .thenReturn(Mono.just(true));

    StepVerifier.create(productService.saveProduct(productRequest))
      .expectError()
      .verify();

  }

  @Test
  void updateProduct() {

    String productIdParam = "id";

    Mockito.when(productRepository.findProductById(Mockito.anyString()))
      .thenReturn(Mono.just(productDefault));

    Mockito.when(productRepository.saveProduct(Mockito.any()))
      .thenReturn(Mono.just(product));

    StepVerifier.create(productService.updateProduct(productRequestDefault, productIdParam))
      .expectNext(product)
      .expectComplete()
      .verify();

  }

  @Test
  void deleteProduct() {

    String productIdParam = "id";

    Mockito.when(productRepository.findExistsProductById(Mockito.anyString()))
      .thenReturn(Mono.just(true));

    Mockito.when(productRepository.deleteProduct(Mockito.any()))
      .thenReturn(Mono.empty());

    StepVerifier.create(productService.deleteProduct(productIdParam))
      .expectComplete()
      .verify();

  }

  @Test
  void deleteProductError() {

    String productIdParam = "id";

    Mockito.when(productRepository.findExistsProductById(Mockito.anyString()))
      .thenReturn(Mono.just(false));

    StepVerifier.create(productService.deleteProduct(productIdParam))
      .expectError()
      .verify();


  }

}
