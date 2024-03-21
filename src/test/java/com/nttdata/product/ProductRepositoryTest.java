package com.nttdata.product;

import com.nttdata.product.enums.ProductTypeEnum;
import com.nttdata.product.model.Product;
import com.nttdata.product.repository.ProductReactiveMongodb;
import com.nttdata.product.repository.impl.ProductRepositoryImpl;
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
public class ProductRepositoryTest {

  private static Product product;


  @InjectMocks
  private ProductRepositoryImpl productRepository;

  @Mock
  private ProductReactiveMongodb productReactiveMongodb;

  @BeforeAll
  static void initialSetup() {
    product = ModelMock.getProduct();
  }

  @Test
  void findProduct() {
    String productTypeParam = ProductTypeEnum.SAVINGS.name();

    Mockito.when(productReactiveMongodb.findByType(Mockito.anyString()))
      .thenReturn(Mono.just(product));

    StepVerifier.create(productRepository.findProduct(productTypeParam))
      .expectNext(product)
      .expectComplete()
      .verify();

    Mockito.verify(productReactiveMongodb, Mockito.times(1))
      .findByType(productTypeParam);
  }

  @Test
  void findProductById() {
    String productIdParam = "id";

    Mockito.when(productReactiveMongodb.findById(Mockito.anyString()))
      .thenReturn(Mono.just(product));

    StepVerifier.create(productRepository.findProductById(productIdParam))
      .expectNext(product)
      .expectComplete()
      .verify();

    Mockito.verify(productReactiveMongodb, Mockito.times(1))
      .findById(productIdParam);
  }

  @Test
  void findExistsProductById() {
    String productIdParam = "id";

    Mockito.when(productReactiveMongodb.existsById(Mockito.anyString()))
      .thenReturn(Mono.just(true));

    StepVerifier.create(productRepository.findExistsProductById(productIdParam))
      .expectNext(true)
      .expectComplete()
      .verify();

    Mockito.verify(productReactiveMongodb, Mockito.times(1))
      .existsById(productIdParam);
  }

  @Test
  void findExistsProduct() {
    String productTypeParam = ProductTypeEnum.SAVINGS.name();

    Mockito.when(productReactiveMongodb.existsByType(Mockito.anyString()))
      .thenReturn(Mono.just(true));

    StepVerifier.create(productRepository.findExistsProduct(productTypeParam))
      .expectNext(true)
      .expectComplete()
      .verify();

    Mockito.verify(productReactiveMongodb, Mockito.times(1))
      .existsByType(productTypeParam);
  }

  @Test
  void saveProduct() {
    Mockito.when(productReactiveMongodb.save(Mockito.any()))
      .thenReturn(Mono.just(product));

    StepVerifier.create(productRepository.saveProduct(product))
      .expectNext(product)
      .expectComplete()
      .verify();

    Mockito.verify(productReactiveMongodb, Mockito.times(1))
      .save(product);
  }

  @Test
  void deleteProduct() {
    String productIdParam = "id";

    Mockito.when(productReactiveMongodb.deleteById(Mockito.anyString()))
      .thenReturn(Mono.empty());

    StepVerifier.create(productRepository.deleteProduct(productIdParam))
      .expectComplete()
      .verify();

    Mockito.verify(productReactiveMongodb, Mockito.times(1))
      .deleteById(productIdParam);
  }

}
