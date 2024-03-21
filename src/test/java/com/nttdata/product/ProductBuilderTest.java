package com.nttdata.product;

import com.nttdata.product.api.request.ProductRequest;
import com.nttdata.product.builder.ProductBuilder;
import com.nttdata.product.model.Product;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
public class ProductBuilderTest {
  @Test
  void initialBuilder() {

    Product product = ProductBuilder.toProductEntity(ModelMock.getProductRequest(),
      ModelMock.getProduct());
    Assertions.assertNotNull(product);
    System.out.println(Product.builder().toString());
    System.out.println(ProductRequest.builder().toString());

  }
}
